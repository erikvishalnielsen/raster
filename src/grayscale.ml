open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun pixel -> 
    match pixel with
    | (r,g,b) -> (let newPix = (r+g+b) / 3 in
    (newPix,newPix,newPix))
    )
;;

let%expect_test "transform grayscale" =
  let myImage = transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm") in
  let correctImage = Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm" in
  let totalWrong = 0 in
  let numIncorrect = Image.foldi myImage ~init:(totalWrong) ~f:(fun ~x:x ~y:y totalWrong pixel -> if not 
    (Pixel.equal pixel (Image.get correctImage ~x:x ~y:y)) then totalWrong + 1 else totalWrong) in
  let output = "Number of Incorrect Pixels: " ^ (Int.to_string numIncorrect) in
  print_endline output;
  [%expect {|Number of Incorrect Pixels: 0|}]
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
