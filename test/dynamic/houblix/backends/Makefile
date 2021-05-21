backends: \
  code \
  code_no_commute \
  code_no_commute_no_stack_optim \
  code_no_inlining \
  code_no_stack_optim \
  old_code \
  table

code:
	@ mkdir -p $@.backend
	@ sed -e "s/FLAGS//" TEMPLATE > $@.backend/dune

code_no_commute:
	@ mkdir -p $@.backend
	@ sed -e "s/FLAGS/--no-push-commute/" TEMPLATE > $@.backend/dune

code_no_commute_no_stack_optim:
	@ mkdir -p $@.backend
	@ sed -e "s/FLAGS/--no-push-commute --no-stack-optimization/" TEMPLATE > $@.backend/dune

code_no_inlining:
	@ mkdir -p $@.backend
	@ sed -e "s/FLAGS/--no-push-commute --no-stack-optimization --no-code-inlining/" TEMPLATE > $@.backend/dune

code_no_stack_optim:
	@ mkdir -p $@.backend
	@ sed -e "s/FLAGS/--no-stack-optimization/" TEMPLATE > $@.backend/dune

old_code:
	@ mkdir -p $@.backend
	@ sed -e "s/FLAGS/--old-code-backend/" TEMPLATE > $@.backend/dune

table:
	@ mkdir -p $@.backend
	@ sed -e "s/FLAGS/--table/" TEMPLATE > $@.backend/dune
