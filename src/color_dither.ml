open Core

(* This should look familiar by now! *)

let isInsideImg ~x ~y ~width ~height = (x >= 0 && x < width) && (y >= 0 && y < height)

let findClosest ~pixList ~pixVal ~interval = 
  let halfway = interval / 2 in

  List.find_exn pixList ~f:(fun colorVal -> if ((Int.abs (pixVal - colorVal)) <= halfway) then true else false)
;;

let transform (image : Image.t) ~n = 
  if n < 2 then failwith "n must be >= 2";

  let maxPixVal = Image.max_val image in
  let imgWidth = Image.width image in
  let imgHeight = Image.height image in
  
  let interval = maxPixVal / (n-1) in
  let pixList = (List.init (n-1) ~f:(fun num -> num * interval)) @ [maxPixVal] in

  Image.foldi image ~init:image ~f:(fun ~x:x ~y:y image pixel -> (
    let thisPixel = ((Pixel.red pixel), (Pixel.green pixel), (Pixel.blue pixel)) in
    let newPixelVal = (findClosest ~pixList:pixList ~pixVal:(Pixel.red pixel) ~interval:interval, findClosest ~pixList:pixList ~pixVal:(Pixel.green pixel) ~interval:interval, 
      findClosest ~pixList:pixList ~pixVal:(Pixel.blue pixel) ~interval:interval) in
    
    Image.set image ~x:x ~y:y newPixelVal;
    let diffPixel = [(Int.to_float((Pixel.red thisPixel) - (Pixel.red (Image.get image ~x:x ~y:y)))); (Int.to_float((Pixel.green thisPixel) - (Pixel.green (Image.get image ~x:x ~y:y))));
      (Int.to_float((Pixel.blue thisPixel) - (Pixel.blue (Image.get image ~x:x ~y:y))))]  in
    
    let functList = [Pixel.red; Pixel.green; Pixel.blue] in

    (*APPLYING ERRORS BELOW*)
    let newrbgList : int list = [] in
     
    if (isInsideImg ~x:(x+1) ~y:(y) ~width:imgWidth ~height:imgHeight) then (

      let newpix = List.fold2_exn diffPixel functList ~init:(newrbgList) ~f:(fun rbgList diff pixFunct -> (
        let currpixelVal = pixFunct (Image.get image ~x:(x+1) ~y:(y)) in
        let newPixelVal = currpixelVal + (Int.of_float (7.0 /. 16.0  *. diff)) in
        rbgList @ [newPixelVal]
      )) in

      Image.set image ~x:(x+1) ~y:(y) (List.nth_exn newpix 0,List.nth_exn newpix 1,List.nth_exn newpix 2
    ));

    if (isInsideImg ~x:(x-1) ~y:(y+1) ~width:imgWidth ~height:imgHeight) then (

      let newpix = List.fold2_exn diffPixel functList ~init:(newrbgList) ~f:(fun rbgList diff pixFunct -> (
        let currpixelVal = pixFunct (Image.get image ~x:(x-1) ~y:(y+1)) in
        let newPixelVal = currpixelVal + (Int.of_float (3.0 /. 16.0  *. diff)) in
        rbgList @ [newPixelVal]
      )) in

      Image.set image ~x:(x-1) ~y:(y+1) (List.nth_exn newpix 0,List.nth_exn newpix 1,List.nth_exn newpix 2
    ));

    if (isInsideImg ~x:(x) ~y:(y+1) ~width:imgWidth ~height:imgHeight) then (

      let newpix = List.fold2_exn diffPixel functList ~init:(newrbgList) ~f:(fun rbgList diff pixFunct -> (
        let currpixelVal = pixFunct (Image.get image ~x:(x) ~y:(y+1)) in
        let newPixelVal = currpixelVal + (Int.of_float (5.0 /. 16.0  *. diff)) in
        rbgList @ [newPixelVal]
      )) in

      Image.set image ~x:(x) ~y:(y+1) (List.nth_exn newpix 0,List.nth_exn newpix 1,List.nth_exn newpix 2
    ));

    if (isInsideImg ~x:(x+1) ~y:(y+1) ~width:imgWidth ~height:imgHeight) then (

      let newpix = List.fold2_exn diffPixel functList ~init:(newrbgList) ~f:(fun rbgList diff pixFunct -> (
        let currpixelVal = pixFunct (Image.get image ~x:(x+1) ~y:(y+1)) in
        let newPixelVal = currpixelVal + (Int.of_float (1.0 /. 16.0  *. diff)) in
        rbgList @ [newPixelVal]
      )) in

      Image.set image ~x:(x+1) ~y:(y+1) (List.nth_exn newpix 0,List.nth_exn newpix 1,List.nth_exn newpix 2
    ));
    
    image
  ))
;;

let%expect_test "color dither" =
  let myImage = transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm") ~n:2 in
  let correctImage = Image.load_ppm ~filename:"../images/reference-beach_portrait_dither_color.ppm" in
  let totalWrong = 0 in
  let numIncorrect = Image.foldi myImage ~init:(totalWrong) ~f:(fun ~x:x ~y:y totalWrong pixel -> if not 
    (Pixel.equal pixel (Image.get correctImage ~x:x ~y:y)) then totalWrong + 1 else totalWrong) in
  let output = "Number of Incorrect Pixels: " ^ (Int.to_string numIncorrect) in
  print_endline output;
  [%expect {|Number of Incorrect Pixels: 0|}]
;;

let command =
  Command.basic
    ~summary:"Color dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and n =
        flag
          "n"
          (required Command.Param.int)
          ~doc:"Number of colors per channel"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~n in
        Image.save_ppm
        image'
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither_color.ppm")]
;;