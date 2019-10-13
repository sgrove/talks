module Option = {
  let getExn =
    fun
    | Some v => v
    | None => raise (Invalid_argument "unwrapUnsafely called on None");
};

let failWith message => raise (Failure message);

exception Foo string;

exception CreditCardMissing;

exception GiftCardMissing;

exception BadGiftCardOffer;

let decorateApiJsObj clientId clientBuildHash clientSessionId now obj =>
  Js.Obj.assign
    {
      "data": Js.null,
      "client_ts": now,
      "client_id": clientId,
      "client_session_id": clientSessionId,
      "build": clientBuildHash
    }
    obj;

let jsonify value => Option.getExn @@ Js.Json.stringifyAny value;

let serverObjOfAction clientId clientBuildHash clientSessionId (action: State.action) => {
  let now = Js.Date.now ();
  let apiObj obj => jsonify @@ decorateApiJsObj clientId clientBuildHash clientSessionId now obj;
  State.(
    switch action {
    | CustomerInquiryInitiated _ =>
      apiObj {"hc/action": "customer-inquiry-initiated", "data": Js.null}
    | CustomerInquiryCleared => apiObj {"hc/action": "customer-inquiry-cleared", "data": Js.null}
    | CustomerInquiryUpdated _ => apiObj {"hc/action": "customer-inquiry-updated", "data": Js.null}
    | CustomerInquiryConfirmed _ =>
      apiObj {"hc/action": "customer-inquiry-confirmed", "data": Js.null}
    | CustomerInquirySubmitted customerInquiry =>
      apiObj {
        "hc/action": "customer-inquiry-submitted",
        "data": {
          "reason":
            switch customerInquiry.reason {
            | None => "unknown"
            | Some reason => State.stringOfCustomerInquiryReason reason
            },
          "gift_card_id":
            switch customerInquiry.giftCardId {
            | None => "unknown"
            | Some id => Uuid.toString id
            },
          "message":
            switch customerInquiry.message {
            | None => "unknown"
            | Some message => message
            }
        }
      }
    | FoundMe maybeMe maybeCards =>
      switch maybeMe {
      | None => apiObj {"hc/action": "open-app", "data": {"email": ""}}
      | Some me =>
        apiObj {
          "hc/action": "open-app",
          "data": {
            "email": me.email,
            "credit_card_count": List.length me.creditCards,
            "gift_card_count":
              switch maybeCards {
              | None => 0
              | Some giftCards => List.length giftCards
              }
          }
        }
      }
    | CreditCardSelected creditCardId =>
      apiObj {"hc/action": "credit-card-selected", "data": {"credit-card-id": creditCardId}}
    | FoundCard maybeCard =>
      switch maybeCard {
      | None =>
        apiObj {
          "hc/action": "refresh-card",
          "data": {"cardId": Js.Json.null, "cardBalance": Js.Json.null}
        }
      | Some gc =>
        apiObj {"hc/action": "refresh-card", "data": {"gcId": gc.id, "gcBalance": gc.balance}}
      }
    | GiftCardPinged giftCardId =>
      apiObj {"hc/action": "gift-card-pinged", "data": {"gift_card_id": giftCardId}}
    | Navigated appScreen =>
      switch appScreen {
      | Inventory =>
        apiObj {"hc/action": "navigated", "data": {"screen": State.stringOfAppScreen appScreen}}
      | Fulfillment giftCardId =>
        apiObj {
          "hc/action": "navigated",
          "data": {"screen": State.stringOfAppScreen appScreen, "gift_card_id": giftCardId}
        }
      | Purchase =>
        apiObj {"hc/action": "navigated", "data": {"screen": State.stringOfAppScreen appScreen}}
      }
    | OfferSelected offer =>
      apiObj {
        "hc/action": "offer-selected",
        "data": {"brand_id": offer.brandId, "balance": offer.balance, "price": offer.price}
      }
    | OfferPurchaseToggled readyToBuyOffer =>
      apiObj {"hc/action": "offer-purchase-toggled", "data": {"ready": readyToBuyOffer}}
    | OfferPurchaseCommitted creditCardId gcBrandId balance sellPrice =>
      apiObj {
        "hc/action": "hc/purchase-card",
        "data": {
          "credit_card_id": creditCardId,
          "balance": balance,
          "sell_price": sellPrice,
          "brand_id": gcBrandId
        }
      }
    | FoundOffers offers =>
      apiObj {
        "hc/action": "found-offers",
        "data": {
          "offers/count":
            switch offers {
            | None => 0
            | Some offers => List.length offers
            }
        }
      }
    | ErrorOccurred kind =>
      apiObj {"hc/action": "client-app-error", "data": {"kind": State.stringOfAppError kind}}
    | GiftCardRemoved card =>
      apiObj {"hc/action": "gift-card-removed", "data": {"card_id": card.id}}
    | OfferPurchaseConfirmed card =>
      apiObj {"hc/action": "offer-purchase-confirmed", "data": {"card_id": card.id}}
    | UriNavigated hash => apiObj {"hc/action": "uri-navigated", "data": {"hash": hash}}
    | Log message => apiObj {"hc/action": "logged", "data": {"message": message}}
    | Alert message => apiObj {"hc/action": "alerted", "data": {"message": message}}
    | DummyAction => apiObj {"hc/action": "dummy", "data": Js.null}
    | Heartbeat => apiObj {"hc/action": "heartbeat", "data": {"time": Js.Date.now ()}}
    }
  )
};

let pluckField decoder field obj => {
  /* Js.log field; */
  ();
  Option.getExn @@ decoder @@ Option.getExn @@ Js.Dict.get obj field
};

let decodeJson str handler => {
  open Js.Json;
  let json =
    try (parseExn str) {
    | _ => failWith "Error parsing API response JSON"
    };
  switch (decodeObject json) {
  | Some obj => handler obj
  | _ => failWith "Couldn't decode JSON properly"
  }
};

let decodeServerError jsonObj =>
  try {
    let error = pluckField Js.Json.decodeString "code" jsonObj;
    State.stringToServerError error
  } {
  | _ => UnknownError "bad-error-format"
  };

module Health = {
  type response = {
    accountCount: int,
    sessionCount: int,
    sessionsHealthy: bool,
    accountsHealthy: bool
  };
  let ofJsonString s =>
    Js.Json.(
      decodeJson
        s
        (
          fun jsonObj => {
            let accountCount = int_of_float @@ pluckField decodeNumber "accounts_count" jsonObj;
            let sessionCount = int_of_float @@ pluckField decodeNumber "sessions_count" jsonObj;
            let accountsHealthy =
              Js.to_bool @@ pluckField decodeBoolean "accounts_healthy" jsonObj;
            let sessionsHealthy =
              Js.to_bool @@ pluckField decodeBoolean "sessions_healthy" jsonObj;
            ({accountCount, sessionCount, sessionsHealthy, accountsHealthy}: response)
          }
        )
    );
};

let parseGiftCardJson cardJson :State.giftCard => {
  open Js.Json;
  let id = Uuid.ofString @@ pluckField decodeString "id" cardJson;
  let brandId = pluckField decodeString "brand" cardJson;
  let userNumber = 0;
  let number = pluckField decodeString "number" cardJson;
  let pin = pluckField decodeString "pin" cardJson;
  let balance = int_of_float @@ pluckField decodeNumber "balance" cardJson;
  let originalBalance = int_of_float @@ pluckField decodeNumber "original_balance" cardJson;
  let barcode =
    try (Some (pluckField decodeString "barcode" cardJson)) {
    | _ => None
    };
  let status = State.cardStatusOfString @@ pluckField decodeString "status" cardJson;
  {id, brand: brandId, userNumber, number, pin, balance, originalBalance, barcode, status}
};

module FindMe = {
  type _response = {user: State.user, giftCards: list State.giftCard};
  type response =
    | Ok _response
    | Error State.serverError;
  let jsonToFindMe jsonObj => {
    open Js.Json;
    let firstName = pluckField decodeString "first_name" jsonObj;
    let lastName = pluckField decodeString "last_name" jsonObj;
    let email = pluckField decodeString "email" jsonObj;
    let apiToken = pluckField decodeString "api_token" jsonObj;
    let creditCards =
      pluckField decodeArray "credit_cards" jsonObj |>
      Array.map (
        fun creditCardJson_ =>
          try {
            let creditCardJson = Option.getExn (decodeObject creditCardJson_);
            (
              {
                id: Uuid.ofString @@ pluckField decodeString "id" creditCardJson,
                lastFour: pluckField decodeString "last_four" creditCardJson,
                brand:
                  State.creditCardBrandOfString @@ pluckField decodeString "brand" creditCardJson,
                expMonth: Some (int_of_float @@ pluckField decodeNumber "exp_month" creditCardJson),
                expYear: Some (int_of_float @@ pluckField decodeNumber "exp_year" creditCardJson)
              }: State.creditCard
            )
          } {
          | Invalid_argument _ => raise CreditCardMissing
          }
      ) |> Array.to_list;
    let giftCards =
      pluckField decodeArray "gift_cards" jsonObj |>
      Array.map (
        fun giftCardJson_ =>
          try {
            let giftCardJson = Option.getExn (decodeObject giftCardJson_);
            parseGiftCardJson giftCardJson
          } {
          | Invalid_argument _ => raise GiftCardMissing
          }
      ) |> Array.to_list;
    (Ok {user: {firstName, lastName, email, creditCards, apiToken}, giftCards}: response)
  };
  let ofJsonString ::log=false s => {
    log ? Js.log ("FindMeResponseJson: " ^ s) : ();
    try (decodeJson s jsonToFindMe) {
    | _ => Error (State.UnknownError "parse-error")
    }
  };
  let executeRequest
      ::log=false
      clientId
      clientBuildHash
      clientSessionId
      successHandler
      errorHandler => {
    let body =
      decorateApiJsObj
        clientId clientBuildHash clientSessionId (Js.Date.now ()) {"hc/query": "hc/find-me"} |> jsonify |> Bs_fetch.BodyInit.make;
    open Bs_promise;
    let headers = Bs_fetch.HeadersInit.make {"this": true};
    Bs_fetch.RequestInit.make credentials::SameOrigin () method_::Post ::body ::headers |>
    Bs_fetch.fetchWithInit "/coffee/api/v1/cc/queries" |>
    andThen Bs_fetch.Response.text |>
    then_ (ofJsonString ::log) |>
    Bs_promise.then_ successHandler |>
    catch (
      fun _error => {
        log ? Js.log _error : ();
        errorHandler (State.FindMeFailed "Not sure")
      }
    )
  };
};

module FindOffers = {
  type response = option (list State.gcBuyOffer);
  let ofJsonString ::log=false s => {
    log ? Js.log ("FindOfferResponse: " ^ s) : ();
    Some Js.Json.(
           decodeJson
             s
             (
               fun jsonObj =>
                 pluckField decodeArray "offers" jsonObj |>
                 Array.map (
                   fun maybeOfferJson =>
                     try {
                       let offerJson = Option.getExn (decodeObject maybeOfferJson);
                       (
                         {
                           balance: int_of_float @@ pluckField decodeNumber "balance" offerJson,
                           price: int_of_float @@ pluckField decodeNumber "price" offerJson,
                           brandId: pluckField decodeString "brand_id" offerJson
                         }: State.gcBuyOffer
                       )
                     } {
                     | Invalid_argument _ => raise BadGiftCardOffer
                     }
                 ) |> Array.to_list
             )
         )
  };
  let executeRequest
      ::log=false
      clientId
      clientBuildHash
      clientSessionId
      successHandler
      errorHandler => {
    let body =
      decorateApiJsObj
        clientId clientBuildHash clientSessionId (Js.Date.now ()) {"hc/query": "hc/find-offers"} |> jsonify |> Bs_fetch.BodyInit.make;
    open Bs_promise;
    let headers = Bs_fetch.HeadersInit.makeWithArray [||];
    Bs_fetch.RequestInit.make credentials::SameOrigin () method_::Post ::body ::headers |>
    Bs_fetch.fetchWithInit "/coffee/api/v1/cc/queries" |>
    andThen Bs_fetch.Response.text |>
    then_ (ofJsonString ::log) |>
    Bs_promise.then_ successHandler |>
    catch (
      fun _error => {
        log ? Js.log _error : ();
        errorHandler (State.FindOffersFailed "Not sure")
      }
    )
  };
};

/*

 */
module PurchaseOffer = {
  type response =
    | Ok State.gcPurchaseConfirmation
    | Error State.serverError;
  let ofJsonString ::log=false s => {
    log ? Js.log ("PurchaseOfferResponseJson: " ^ s) : ();
    try
      Js.Json.(
        decodeJson
          s
          (
            fun jsonObj => {
              let cardJson = pluckField decodeObject "card" jsonObj;
              let giftCard = parseGiftCardJson cardJson;
              (Ok {giftCard: giftCard}: response)
            }
          )
      ) {
    | _ => Error (decodeJson s decodeServerError)
    }
  };
  let executeRequest
      ::log=false
      (clientId: string)
      clientBuildHash
      clientSessionId
      creditCardId
      (purchaseOffer: State.gcBuyOffer)
      (successHandler: response => unit)
      (errorHandler: State.appError => unit)
      :Bs_promise.t 'e 'f => {
    let body =
      decorateApiJsObj
        clientId
        clientBuildHash
        clientSessionId
        (Js.Date.now ())
        {
          "hc/action": "hc/purchase-card",
          "data": {
            "credit_card_id": creditCardId,
            "balance": purchaseOffer.balance,
            "price": purchaseOffer.price,
            "brand_id": purchaseOffer.brandId
          }
        } |> jsonify |> Bs_fetch.BodyInit.make;
    open Bs_promise;
    let headers = Bs_fetch.HeadersInit.makeWithArray [||];
    Bs_fetch.RequestInit.make credentials::SameOrigin () method_::Post ::body ::headers |>
    Bs_fetch.fetchWithInit "/coffee/api/v1/cc/effects" |>
    andThen Bs_fetch.Response.text |>
    then_ (ofJsonString ::log) |>
    Bs_promise.then_ successHandler |>
    catch (
      fun _error => {
        log ? Js.log _error : ();
        errorHandler (State.OfferPurchaseFailed (State.UnknownError "parse-error"))
      }
    )
  };
};

module ReportTransition = {
  type response = State.reportTransitionConfirmation;
  let ofJsonString ::log=false s => {
    log ? Js.log ("ReportTransitionResponseJson: " ^ s) : ();
    Js.Json.(
      decodeJson
        s
        (
          fun jsonObj => {
            let success =
              try (Js.to_bool @@ pluckField decodeBoolean "success" jsonObj) {
              | _ => false
              };
            ({success: success}: response)
          }
        )
    )
  };
  let executeRequest ::log=false jsonStringBody successHandler errorHandler => {
    open Bs_promise;
    let body = Bs_fetch.BodyInit.make jsonStringBody;
    let headers = Bs_fetch.HeadersInit.makeWithArray [||];
    Bs_fetch.RequestInit.make credentials::SameOrigin () method_::Post ::body ::headers |>
    Bs_fetch.fetchWithInit "/coffee/api/v1/cc/events" |>
    andThen Bs_fetch.Response.text |>
    then_ (ofJsonString ::log) |>
    Bs_promise.then_ successHandler |>
    catch (
      fun _error => {
        log ? Js.log _error : ();
        errorHandler (State.ReportTransitionFailed "Not sure")
      }
    )
  };
};

module QueryCard = {
  type response =
    | Ok State.gcRefresh
    | Error State.serverError;
  let ofJsonString ::log=false s => {
    log ? Js.log ("QueryCardResponseJson: " ^ s) : ();
    try
      Js.Json.(
        decodeJson
          s
          (
            fun jsonObj => {
              let cardJson = pluckField decodeObject "giftcard" jsonObj;
              let giftCard = parseGiftCardJson cardJson;
              Ok {giftCard: giftCard}
            }
          )
      ) {
    | _ => Error (decodeJson s decodeServerError)
    }
  };
  let executeRequest
      ::log=false
      clientId
      clientBuildHash
      clientSessionId
      (giftCardId: State.gcId)
      successHandler
      errorHandler => {
    let body =
      decorateApiJsObj
        clientId
        clientBuildHash
        clientSessionId
        (Js.Date.now ())
        {"hc/query": "hc/find-card", "data": {"card_id": giftCardId}} |> jsonify |> Bs_fetch.BodyInit.make;
    open Bs_promise;
    let headers = Bs_fetch.HeadersInit.make {"this": true};
    Bs_fetch.RequestInit.make credentials::SameOrigin () method_::Post ::body ::headers |>
    Bs_fetch.fetchWithInit "/coffee/api/v1/cc/queries" |>
    andThen Bs_fetch.Response.text |>
    then_ (ofJsonString ::log) |>
    Bs_promise.then_ successHandler |>
    catch (
      fun _error => {
        log ? Js.log _error : ();
        errorHandler (State.QueryCardFailed (State.UnknownError "parsing-error"))
      }
    )
  };
};

module SubmitCustomerInquiry = {
  type response =
    | Ok bool
    | Error State.serverError;
  let ofJsonString ::log=false (s: string) :response => {
    log ? Js.log ("SubmitCustomerInquiryResponseJson: " ^ s) : ();
    try
      Js.Json.(
        decodeJson
          s
          (
            fun jsonObj => {
              let success = Js.to_bool @@ pluckField decodeBoolean "success" jsonObj;
              Ok success
            }
          )
      ) {
    | _ => Error (decodeJson s decodeServerError)
    }
  };
  let executeRequest
      ::log=false
      clientId
      clientBuildHash
      clientSessionId
      (inquiry: State.customerInquiry)
      successHandler
      errorHandler => {
    let reason =
      switch inquiry.reason {
      | None => Js.Json.null
      | Some reason => Js.Json.string (State.stringOfCustomerInquiryReason reason)
      };
    let giftCardId =
      switch inquiry.giftCardId {
      | None => Js.Json.null
      | Some id => Js.Json.string (Uuid.toString id)
      };
    let message =
      switch inquiry.message {
      | None => Js.Json.null
      | Some message => Js.Json.string message
      };
    let body =
      decorateApiJsObj
        clientId
        clientBuildHash
        clientSessionId
        (Js.Date.now ())
        {
          "hc/action": "hc/submit-customer-inquiry",
          "data": {"reason": reason, "gift_card_id": giftCardId, "message": message}
        } |> jsonify |> Bs_fetch.BodyInit.make;
    open Bs_promise;
    let headers = Bs_fetch.HeadersInit.make {"this": true};
    Bs_fetch.RequestInit.make credentials::SameOrigin () method_::Post ::body ::headers |>
    Bs_fetch.fetchWithInit "/coffee/api/v1/cc/effects" |>
    andThen Bs_fetch.Response.text |>
    then_ (ofJsonString ::log) |>
    Bs_promise.then_ successHandler |>
    catch (
      fun _error => {
        log ? Js.log _error : ();
        ignore (
          Bs_promise.reject (
            State.SubmitCustomerInquiryFailed (State.UnknownError "parsing-error")
          )
        );
        errorHandler (State.SubmitCustomerInquiryFailed (State.UnknownError "parsing-error"))
      }
    )
  };
};
