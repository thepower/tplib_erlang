%This file was generated automatically by bin/generate_headers script
% from priv/tx_const.json.
% You will lose all custom changes in this file.
decode_purpose(3) -> gas;
decode_purpose(2) -> dstfee;
decode_purpose(1) -> srcfee;
decode_purpose(0) -> transfer;
decode_purpose(N) when is_integer(N) -> N.
encode_purpose(gas) -> 3;
encode_purpose(dstfee) -> 2;
encode_purpose(srcfee) -> 1;
encode_purpose(transfer) -> 0;
encode_purpose(N) when is_integer(N) -> N.
decode_kind(23) -> {2,notify};
decode_kind(22) -> {2,lstore};
decode_kind(21) -> {2,tstore};
decode_kind(20) -> {2,block};
decode_kind(19) -> {2,patch};
decode_kind(18) -> {2,deploy};
decode_kind(17) -> {2,register};
decode_kind(16) -> {2,generic};
decode_kind(Any) -> throw({unknown_kind_id,Any}).
encode_kind(2,notify) -> 23;
encode_kind(2,lstore) -> 22;
encode_kind(2,tstore) -> 21;
encode_kind(2,block) -> 20;
encode_kind(2,patch) -> 19;
encode_kind(2,deploy) -> 18;
encode_kind(2,register) -> 17;
encode_kind(2,generic) -> 16;
encode_kind(Ver,Any) -> throw({unknown_kind,Ver,Any}).
