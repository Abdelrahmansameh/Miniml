open Ast;;

val p_name: out_channel->name->unit;;
val p_const: out_channel->const->unit;;
val p_uop: out_channel->uop->unit;;
val p_bop: out_channel->bop->unit;;
val p_typ: out_channel->typ->unit;;
val p_arg: out_channel->arg->unit;;
val p_expr: out_channel->expr->unit;;
