open Core

(* This should look familiar by now! *)

let isInsideImg ~x ~y ~width ~height = (x >= 0 && x < width) && (y >= 0 && y < height)
let transform image = 
  let grayscaleImg = Grayscale.transform image in
  let maxPixVal = Image.max_val grayscaleImg in
  let imgWidth = Image.width grayscaleImg in
  let imgHeight = Image.height grayscaleImg in
  Image.foldi grayscaleImg ~init:grayscaleImg ~f:(fun ~x:x ~y:y grayscaleImg pixel -> (
    let pixelval = (Pixel.red pixel) in 
    if pixelval >= (maxPixVal / 2) then Image.set grayscaleImg ~x:x ~y:y (maxPixVal,maxPixVal,maxPixVal) else Image.set grayscaleImg ~x:x ~y:y (0,0,0);
    let diff = Int.to_float(pixelval - (Pixel.red (Image.get grayscaleImg ~x:x ~y:y))) in

    (*APPLYING ERRORS BELOW*)
    (*TO THE RIGHT*)
    if (isInsideImg ~x:(x+1) ~y:(y) ~width:imgWidth ~height:imgHeight) then (
      let currpixelVal = Pixel.red (Image.get grayscaleImg ~x:(x+1) ~y:(y)) in
      let newPixelVal = currpixelVal + (Int.of_float (7.0 /. 16.0  *. diff)) in
      Image.set grayscaleImg ~x:(x+1) ~y:(y) (newPixelVal,newPixelVal,newPixelVal));
    (*TO THE BOTTOM LEFT*)
    if (isInsideImg ~x:(x-1) ~y:(y+1) ~width:imgWidth ~height:imgHeight) then (
      let currpixelVal = Pixel.red (Image.get grayscaleImg ~x:(x-1) ~y:(y+1)) in
      let newPixelVal = currpixelVal + (Int.of_float (3.0 /. 16.0  *. diff)) in
      Image.set grayscaleImg ~x:(x-1) ~y:(y+1) (newPixelVal,newPixelVal,newPixelVal));
    (*TO THE BOTTOM*)
    if (isInsideImg ~x:(x) ~y:(y+1) ~width:imgWidth ~height:imgHeight) then (
      let currpixelVal = Pixel.red (Image.get grayscaleImg ~x:(x) ~y:(y+1)) in
      let newPixelVal = currpixelVal + (Int.of_float (5.0 /. 16.0  *. diff)) in
      Image.set grayscaleImg ~x:(x) ~y:(y+1) (newPixelVal,newPixelVal,newPixelVal));
    (*TO THE BOTTOM RIGHT*)
    if (isInsideImg ~x:(x+1) ~y:(y+1) ~width:imgWidth ~height:imgHeight) then (
      let currpixelVal = Pixel.red (Image.get grayscaleImg ~x:(x+1) ~y:(y+1)) in
      let newPixelVal = currpixelVal + (Int.of_float (1.0 /. 16.0  *. diff)) in
      Image.set grayscaleImg ~x:(x+1) ~y:(y+1) (newPixelVal,newPixelVal,newPixelVal));
    
    grayscaleImg
  ))
;;

let%expect_test "transform dither" =
  let myImage = transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm") in
  let correctImage = Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm" in
  let totalWrong = 0 in
  let numIncorrect = Image.foldi myImage ~init:(totalWrong) ~f:(fun ~x:x ~y:y totalWrong pixel -> if not 
    (Pixel.equal pixel (Image.get correctImage ~x:x ~y:y)) then totalWrong + 1 else totalWrong) in
  let output = "Number of Incorrect Pixels: " ^ (Int.to_string numIncorrect) in
  print_endline output;
  [%expect {|Number of Incorrect Pixels: 0|}]
;;


let command =
  Command.basic
    ~summary:"Dither an image"
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
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
