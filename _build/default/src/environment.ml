type dimensions = {
  width : int;
  height : int;
}

type env = { dimensions : dimensions }

let height ob = ob.dimensions.height
let width ob = ob.dimensions.width
let make width height = { dimensions = { width; height } }
