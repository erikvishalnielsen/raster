open Core

(* This should look familiar by now! *)

let isInsideImg ~x ~y ~width ~height = (x >= 0 && x < width) && (y >= 0 && y < height)

let msqError ~currRegion ~compRegion = 
  Image.foldi currRegion ~init:(0) ~f:(fun ~x:x ~y:y msqErr currPixel ->
    let compPixel = Image.get compRegion ~x:x ~y:y in
    let redVal = ((Pixel.red currPixel) - (Pixel.red compPixel)) in 
    let greenVal = ((Pixel.red currPixel) - (Pixel.red compPixel)) in 
    let blueVal = ((Pixel.red currPixel) - (Pixel.red compPixel)) in 
    msqErr + (redVal * redVal) + (greenVal * greenVal) + (blueVal * blueVal)
  )
;;

let imageRegions ~image ~numWidth ~numHeight ~width ~height = 
  (List.init numWidth ~f:(fun x -> 
    (List.init numHeight ~f:(fun y -> (Image.slice image ~x_start:(x * width) ~x_end:((x+1) * width) ~y_start:(y * height) ~y_end:((y+1) * height))))
  ))
;;

let swapRegions ~image ~width ~height ~currX ~currY ~currRegion ~otherX ~otherY ~otherRegion =
  List.init (width * height) ~f:(fun i -> 
    let x1 = currX + (i % width) in
  )
  image
;;

let rec mosiacRec ~image ~width ~height ~imgRegions ~moves = 
  match moves with
  | 0 -> image
  | _ -> (
  let numWidth = (Image.width image) / width in
  let numHeight = (Image.height image) / height in
  let indexX = Random.int (numWidth) in
  let indexY = Random.int (numHeight) in
  let currRegion = List.nth_exn (List.nth_exn imgRegions indexX) indexY in

  (* 1st xcoord 2nd ycoord and 3rd msqError *)
  let lowestMSQ = List.foldi imgRegions ~init:(-1,-1,Int.max_value) ~f:(fun x currLowest imgRow -> (
    List.foldi imgRow ~init:(currLowest) ~f:(fun y currLowest imgSect -> (
      let msqError = msqError ~currRegion:currRegion ~compRegion:imgSect in
      if (not (x = indexX && y = indexY)) then (match currLowest with
        | (_,_,msq) -> if msqError < msq then (x,y,msqError) else currLowest
      ) else currLowest
    ))
  )) in

  let newImage = match lowestMSQ with 
  | (x,y,region) -> swapRegions ~image:image ~width:width ~height:height ~currX:(indexX*width) ~currY:(indexY*height) ~currRegion:currRegion ~otherX:(x*width) ~otherY:(y*height) ~otherRegion:region
in

  mosiacRec ~image:newImage ~width:width ~height:height ~imgRegions:(imageRegions ~image:image ~numWidth:numWidth ~numHeight:numHeight ~width:width ~height:height) ~moves:(moves-1);
  )
;;

let transform (image : Image.t) ~width ~height ~moves = 
  let imgWidth = Image.width image in
  let imgHeight = Image.height image in 
  if not (imgWidth % width = 0 && imgHeight % height = 0) then failwith "Width and Height must go perfectly into the Width and Height of the image";

  let numWidth = imgWidth / width in
  let numHeight = imgHeight / width in

  let imgRegions = (List.init numWidth ~f:(fun x -> 
    (List.init numHeight ~f:(fun y -> (Image.slice image ~x_start:(x * width) ~x_end:((x+1) * width) ~y_start:(y * height) ~y_end:((y+1) * height))))
  )) in

  mosiacRec ~image:image ~width:width ~height:height ~imgRegions:imgRegions ~moves:moves
;;

let command =
  Command.basic
    ~summary:"Mosaic of an Image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and width =
        flag
          "width"
          (required Command.Param.int)
          ~doc:"Width of regions"
      and height =
        flag
          "height"
          (required Command.Param.int)
          ~doc:"Height of regions"
      and moves =
          flag
          "moves"
          (required Command.Param.int)
          ~doc:"Number of moves"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~width ~height ~moves in
        Image.save_ppm
        image'
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;