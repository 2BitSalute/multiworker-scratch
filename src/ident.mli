(*
 * Copyright (c) 2022, Tatiana Racheva
 * Copyright (c) 2015, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module MakeIdent
    (Core: Core_sig.COREOPS)
    (IMap: Collections_sig.MAP with type key = int)
  : Ident_sig.IDENT