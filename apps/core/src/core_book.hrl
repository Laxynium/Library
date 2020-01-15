-record (book_info,{
    title::string(), 
    author::string(), 
    version::integer()
}).
-record(check_out_info, {
    since :: calendar:date(), 
    till :: calendar:date(),
    by :: lib_user:user_card_id(),
    returned :: boolean(),
    returned_at :: calendar:date() | {}
}).
-record (book, {
    id :: book_id(), 
    book_info :: book_info(),
    check_out_info :: [check_out_info()]
}).