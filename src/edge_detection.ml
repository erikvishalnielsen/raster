open Core

(* This should look familiar by now! *)

(*THIS FUNCTION RULES OUT BORDERS*)
let isNotBorders ~x ~y ~width ~height = (x > 0 && x < width-1) && (y > 0 && y < height-1)

(*THRESHOLD SHOULD BE BETWEEN 0-1*)
let transform image ~threshold = 
  if (Float.(>.) threshold 1.0 || Float.(<.) threshold 0.0) then failwith "Threshold should be a decimal 0-1";
  let gx = [[-1;0;1]; [-2;0;2]; [-1;0;1]] in
  let gy = [[-1;-2;-1]; [0;0;0]; [1;2;1]] in
  let blurGrayImage = Blur.transform (Grayscale.transform image) ~radius:2 in
  let maxPixVal = Image.max_val blurGrayImage in
  let imgWidth = Image.width blurGrayImage in
  let imgHeight = Image.height blurGrayImage in
  let thresholdVal = Float.to_int (threshold *. (Int.to_float maxPixVal)) in
  
  (*IGNORING EDGE CASES*)
  Image.mapi blurGrayImage ~f:(fun ~x:x ~y:y _pixel -> 
    if (isNotBorders ~x:x ~y:y ~width:imgWidth ~height:imgHeight) then (
      let imageSlice = Image.slice blurGrayImage ~x_start:(x-1) ~x_end:(x+2) ~y_start:(y-1) ~y_end:(y+2) in
      let gxinit = 0 in
      let gyinit = 0 in
      let gxVal = Image.foldi imageSlice ~init:(gxinit) ~f:(fun ~x:x ~y:y gxinit pixel -> 
        gxinit + ((List.nth_exn (List.nth_exn gx x) y) * (Pixel.red pixel))
      ) in
      let gyVal = Image.foldi imageSlice ~init:(gyinit) ~f:(fun ~x:x ~y:y gyinit pixel -> 
        gyinit + ((List.nth_exn (List.nth_exn gy x) y) * (Pixel.red pixel))
      ) in
      let gValue = Float.to_int (Float.sqrt (((Int.to_float gxVal) *.(Int.to_float gxVal)) +. ((Int.to_float gyVal) *. (Int.to_float gyVal)))) in
      if (gValue >= thresholdVal) then (maxPixVal,maxPixVal,maxPixVal) else (0,0,0)
    ) else (0,0,0)
  )
;;

let command =
  Command.basic
    ~summary:"Edge detection an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and threshold =
        flag
          "threshold"
          (required Command.Param.float)
          ~doc:"Threshold needed for edge identification"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~threshold in
        Image.save_ppm
        image'
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge_detection.ppm")]
;;