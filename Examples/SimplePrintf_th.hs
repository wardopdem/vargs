class ArgPrc_printf_aa3M a where
  prc_printf_aa3O :: String -> [PfValWrap] -> a

class ArgSrc_printf_aa3N a where
  toArg_printf_aa3Q :: a -> PfValWrap

instance ArgPrc_printf_aa3M String where
  prc_printf_aa3O a1 = printf_String a1 . reverse

instance ArgPrc_printf_aa3M (IO ()) where
  prc_printf_aa3O a1 = printf_IO a1 . reverse

instance (ArgSrc_printf_aa3N a, ArgPrc_printf_aa3M r) => ArgPrc_printf_aa3M (a -> r) where
  prc_printf_aa3O a1 acc_printf_aa3P
    = prc_printf_aa3O a1 . (: acc_printf_aa3P) . toArg_printf_aa3Q

instance ArgSrc_printf_aa3N PfValWrap where
  toArg_printf_aa3Q = id

instance ArgSrc_printf_aa3N Integer where
  toArg_printf_aa3Q = Val

instance ArgSrc_printf_aa3N Double where
  toArg_printf_aa3Q = Val

instance ArgSrc_printf_aa3N String where
  toArg_printf_aa3Q = Val

printf :: forall a. ArgPrc_printf_aa3M a => String -> a
printf a1 = prc_printf_aa3O a1 []

