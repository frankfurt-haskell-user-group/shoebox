enum FileCommand {
	GetCurrentDB;
	GetAvailableDBs;
	CreateDB Text;
	DeleteDB Text;
	OpenDB Text;
	SaveDB;
	SaveDBAs Text;
}

enum QueryCommand {
     DbInfo;
     DbQuery Text;
}

enum Command {
     	NoCommand;
        CmdFc FileCommand;
	CmdQuery QueryCommand;
}
