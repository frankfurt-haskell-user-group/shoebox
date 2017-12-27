// response to file operations
enum FileResponse {
     CurrentDBChanged Text;    // the DB in use just changed
     AvailableDBs List(Text);  // list of DB's on disk
     OpenedDB Text;            // just opened the DB
     CreatedDB Text;           // just created the DB
     DeletedDB Text;           // just deleted the DB
     SavedDB Text;             // just saved the DB
}

// response to database queries
enum QueryResponse {
     DbInfo Text;              // detailed DB info as JSON text
     DbQuery Text;             // query answer as JSON text
}

// response to commands
enum Response {
     NoResponse;               // response to NoOP command (is this needed?)
     ResFr FileResponse;       // response to a file command
     ResQuery QueryResponse;   // response to a query command
     TestAnswer Text;          // arbitrary test answer as text
}
