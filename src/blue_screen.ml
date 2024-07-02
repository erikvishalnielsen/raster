open Core

(* You need to change the implementation of this function so that it
   replaces the "blue" pixels of the foreground image with pixels from
   the corresponding position in the background image instead of
   just ignoring the background image and returning the foreground image.
*)
let transform ~foreground ~background = 
  Image.mapi foreground ~f:(fun ~x:x ~y:y pixel -> 
    let boolean = (match pixel with
    | (r,g,b) -> b > (r+g))
  in
    if boolean then (Image.get background ~x:x ~y:y) else pixel
    )
;;

let%expect_test "transform bluescreen" =
  let myImage = transform ~foreground:(Image.load_ppm ~filename:"../images/oz_bluescreen.ppm") ~background:(Image.load_ppm ~filename:"../images/meadow.ppm") in

  let correctImage = Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm" in
  let totalWrong = 0 in
  let numIncorrect = Image.foldi myImage ~init:(totalWrong) ~f:(fun ~x:x ~y:y totalWrong pixel -> if not 
    (Pixel.equal pixel (Image.get correctImage ~x:x ~y:y)) then totalWrong + 1 else totalWrong) in
  let output = "Number of Incorrect Pixels: " ^ (Int.to_string numIncorrect) in
  print_endline output;
  [%expect {|Number of Incorrect Pixels: 0|}]
;;

let command =
  Command.basic
    ~summary:"Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:(String.chop_suffix_exn foreground_file ~suffix:".ppm" ^ "_vfx.ppm")]
;;
