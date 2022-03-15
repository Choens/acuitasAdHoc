# Update documentation.
build:
	Rscript -e 'devtools::build(".")'

check:
	Rscript -e 'devtools::check(".")'

document: 
	Rscript -e 'devtools::document()'

install: 
	Rscript -e 'remotes::install()'

install_gitlab: 
	Rscript -e 'remotes::install_gitlab("acuitas-public/acuitasadhoc", force = TRUE)'

release:
	Rscript -e 'devtools::release(".")'

release_checks:
	Rscript -e 'devtools::release_checks(".")'

run_examples: 
	Rscript -e 'devtools::run_examples()'

spell_check: 
	Rscript -e 'devtools::spell_check()'

test: 
	Rscript -e 'devtools::test()'

.PHONY: check document install
