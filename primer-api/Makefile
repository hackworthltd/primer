swagger-targets = static-swgen-typescript static-swgen-typescript-angular static-swgen-typescript-angularjs-deprecated static-swgen-typescript-aurelia static-swgen-typescript-axios static-swgen-typescript-fetch static-swgen-typescript-inversify static-swgen-typescript-jquery static-swgen-typescript-nestjs static-swgen-typescript-node static-swgen-typescript-redux-query static-swgen-typescript-rxjs

.PHONY: static-swagger-gen $(swagger-targets)
.ONESHELL: $(swagger-targets)

static-swagger-gen : static-swgen-typescript $(swagger-targets)

.PHONY : swagger.json
swagger.json : 
	cabal run -v0 swagger > $@

$(swagger-targets) : static-swgen-% : swagger.json
	openapi-generator-cli generate -i $< -o ts-swagger-generated/$* -g $*
