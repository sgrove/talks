# pdx


[![CircleCI](https://circleci.com/gh/yourgithubhandle/pdx/tree/master.svg?style=svg)](https://circleci.com/gh/yourgithubhandle/pdx/tree/master)


**Contains the following libraries and executables:**

```
pdx@0.0.0
│
├─test/
│   name:    TestPdx.exe
│   main:    TestPdx
│   require: pdx.lib
│
├─library/
│   library name: pdx.lib
│   namespace:    Pdx
│   require:
│
└─executable/
    name:    PdxApp.exe
    main:    PdxApp
    require: pdx.lib
```

## Developing:

```
npm install -g esy
git clone <this-repo>
esy install
esy build
```

## Running Binary:

After building the project, you can run the main binary that is produced.

```
esy x PdxApp.exe 
```

## Running Tests:

```
# Runs the "test" command in `package.json`.
esy test
```
