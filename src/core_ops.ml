module CoreOps : Core_sig.COREOPS = struct
  module Array = struct
    let length = Array.length
    let of_list = Array.of_list
    let sub array ~pos ~len = Array.sub array pos len
    let to_list = Array.to_list
  end
end