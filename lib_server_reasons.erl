-module(lib_server_reasons).
-export([noUser/0,noBook/0,bookBorrowed/0,userCantBorrow/0,notBorrowedByUser/0,alreadyBorrowed/0]).

noUser() -> "User not in system".
noBook() -> "Book not in library".
bookBorrowed() -> "Book is already borrowed".
userCantBorrow() -> "This user can't currenly borrow books".
notBorrowedByUser() -> "This book is not borrowed by this user".
alreadyBorrowed() -> "This book is already borrowed".
