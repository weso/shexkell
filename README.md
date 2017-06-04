# shexkell
ShEx implementation in Haskell

## Environment ##
Clone the repo and cd to it. In order to build the development environment:
 ``` sh
 $ stack setup
 $ stack build
 ```

 This will download all the necessary dependencies for the project

## Running Shexkell ##
Shexkell comes with a *Command Line Interface* to validate nodes of a given graph agains a given schema, with a given
Shape Map. To run it, pass the folowing arguments to the executable:
```sh
$ shexkell -map <Shape Map path> -rdf <graph path> -shex <ShEx schema path>
```

The following flags can be added to specify the format of the files:
* ```--compact``` Parse the ShEx schema as ShExC
* ```--json``` Parse the ShEx schema as JSONLD (ShExJ)

## Test suite ##

### Unit tests ###
To run the unit tests:
``` sh
$ stack test
```

### Compatibility tests ###
To run the compatibility tests you must download the [test suite](https://github.com/shexSpec/shexTest/) and reference it in a configuration file with the format:
``` json
{
  "basePath": "/home/sergio/foo/shexTest",
  "manifestPath": "manifest.jsonld",
  "casesToRun": [
    "#1dot_fail-missing"
  ]
}
```
* The field `"basePath"` specifies the base path of the test suite
* The field `"manifestPath"` specifies the name of the [manifest](https://github.com/shexSpec/shexTest/blob/master/validation/manifest.jsonld) file to load
* The field `"casesToRun"` specifies the IDs of the test cases to run as referenced in the [manifest](https://github.com/shexSpec/shexTest/blob/master/validation/manifest.jsonld). If the field is not present, all cases are run

After saving the configuration file, run:
``` sh
$ stack test --test-arguments='--compat testsConfig.json'
```