// module, to store the commands this frontend sends and receives
import CBOR from './other_libs/cbor';

(function (global, undefined) { "use strict";


class CborStructItem {

    constructor(...args) {
       this.setValue(...args);
    }

    setValue(...args) {
       this.record = args;
    }

    fromCBOR (ser_data) {
       var json_data = CBOR.decode(ser_data);
       this.fromData(json_data);
       return this;
    }

    toCBOR () {
       var data = this.toData();
       return CBOR.encode(data);
    }

    toData () {
       return this.record;
    }

    fromData (json_data) {
       this.setValue(...json_data);
       return this;
    }
}

class CborEnumItem extends CborStructItem {
    setValue(...args) {
       if (args.length > 0) {
         this.selector = args[0];
         this.record = args.slice(1);
       }
    }

    toData () {
       return [this.selector, ...this.record];
    }
}

// commands for file operations
class FileCommand extends CborEnumItem {
}

FileCommand.GetCurrentDB = 0;   // show DB in use
FileCommand.GetAvailableDBs = 1;   // get list of all DB's
FileCommand.CreateDB = 2;   // create a new DB
FileCommand.DeleteDB = 3;   // delete an existing DB
FileCommand.OpenDB = 4;   // open an existing DB
FileCommand.SaveDB = 5;   // save the DB in use
FileCommand.SaveDBAs = 6;   // save the DB in use with a new name

// commands for database query operations
class QueryCommand extends CborEnumItem {
}

QueryCommand.DbInfo = 0;   // get detailed info on a database
QueryCommand.DbQuery = 1;   // query a database for an entry
QueryCommand.WordQuery = 2;   // (id of query) (word of query)
QueryCommand.QueryTransCols = 3;   // query column information (translation steps)
QueryCommand.QueryTransWord = 4;   // query translation of word, id, word
QueryCommand.QueryInsertWord = 5;   // query insertion of word: id, word, translation, db, memo

// all possible commands for the shoebox module
// gathered from the different command sub types
class Command extends CborEnumItem {
    toData () {
       var arr_in = this.record.slice(); var arr_out = [];
       if (this.selector == 0) {
       }
       if (this.selector == 1) {
            arr_out.push(arr_in.shift().toData());
       }
       if (this.selector == 2) {
            arr_out.push(arr_in.shift().toData());
       }
       if (this.selector == 3) {
            arr_out.push(arr_in.shift());
       }
       return [this.selector, ...arr_out];
    }

     fromData (json_data) {
       var arr_in = json_data.slice(1); var arr_out = [];
       if (json_data[0] == 0) {
       }
       if (json_data[0] == 1) {
            arr_out.push((new FileCommand()).fromData(arr_in.shift()));
       }
       if (json_data[0] == 2) {
            arr_out.push((new QueryCommand()).fromData(arr_in.shift()));
       }
       if (json_data[0] == 3) {
            arr_out.push(arr_in.shift());
       }
       this.selector = json_data[0];
       this.record = arr_out;
       return this;
     }
}

Command.NoCommand = 0;   // no action requested
Command.CmdFc = 1;   // one of the file commands
Command.CmdQuery = 2;   // one of the query commands
Command.RunTest = 3;   // arbitrary text, send as test command


module.exports = { QueryCommand: QueryCommand, FileCommand: FileCommand, Command: Command };

}) (this);

