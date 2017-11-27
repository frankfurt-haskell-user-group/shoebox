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
     DbInfo;         // get detailed info on a database
     DbQuery Text;   // query a database for an entry
}

// all possible commands for the shoebox module
// gathered from the different command sub types
enum Command {
     	NoCommand;               // no action requested
        CmdFc FileCommand;       // one of the file commands
	CmdQuery QueryCommand;   // one of the query commands
}
