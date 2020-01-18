-module(lib_server_reasons).
-compile([export_all]).

noUser() -> "User not in system".
noBook() -> "Book not in library".
bookBorrowed() -> "Book is already borrowed".
userCantBorrow() -> "This user can't currenly borrow books".
notBorrowedByUser() -> "This book is not borrowed by this user".
alreadyBorrowed() -> "This book is already borrowed".
tooLateToExtend() -> "It's too late to extend".
bookAlreadyExists() -> "There already exists book with that id".
userAlreadyExists() -> "There already exists user with that id".
