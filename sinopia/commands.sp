// commands for file operations
enum FileCommand {
	GetCurrentDB;     // show DB in use
	GetAvailableDBs;  // get list of all DB's
	CreateDB Text;    // create a new DB
	DeleteDB Text;    // delete an existing DB
	OpenDB Text;      // open an existing DB
	SaveDB;           // save the DB in use
	SaveDBAs Text;    // save the DB in use with a new name
}

// commands for database query operations
enum QueryCommand {
  DbInfo;          // get detailed info on a database
  DbQuery Text;    // query a database for an entry
  WordQuery Text Text; // (id of query) (word of query)
  QueryTransCols;  // query column information (translation steps)
  QueryTransWord Text Text;  // query translation of word, id, word
  QueryInsertWord Text Text List(Text) Text Text;  // query insertion of word: id, word, translation, db, memo
}

// all possible commands for the shoebox module
// gathered from the different command sub types
enum Command {
  NoCommand;               // no action requested
  CmdFc FileCommand;       // one of the file commands
	CmdQuery QueryCommand;   // one of the query commands
  RunTest Text;     // arbitrary text, send as test command
}
