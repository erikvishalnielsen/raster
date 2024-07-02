open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius = 
  let imgWidth = Image.width image in
  let imgHeight = Image.height image in 
  Image.mapi image ~f:(fun ~x:x ~y:y _pixel -> 
    Image.mean_pixel (Image.slice image ~x_start:(if (x-radius < 0) then 0 else x-radius) ~x_end:(if (x+radius >= imgWidth) then imgWidth-1 else x+radius)
    ~y_start:(if (y-radius < 0) then 0 else y-radius) ~y_end:(if (y+radius >= imgHeight) then imgHeight-1 else y+radius)
  ))
;;

let%expect_test "transform blur" =
  let myImage = transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm") ~radius:3 in
  let correctImage = Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm" in
  let totalWrong = 0 in
  let numIncorrect = Image.foldi myImage ~init:(totalWrong) ~f:(fun ~x:x ~y:y totalWrong pixel -> if not 
    (Pixel.equal pixel (Image.get correctImage ~x:x ~y:y)) then totalWrong + 1 else totalWrong) in
  let output = "Number of Incorrect Pixels: " ^ (Int.to_string numIncorrect) in
  print_endline output;
  [%expect {|Number of Incorrect Pixels: 0|}]
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
