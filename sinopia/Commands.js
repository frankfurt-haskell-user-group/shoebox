import CBOR from '../other_libs/cbor';

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
    }

    toCBOR () {
       var data = this.toData();
       return CBOR.encode(data);
    }

    toData () {
       return this.record;
    }

    fromData (json_data) {
       this.setValue(json_data);
    }
}

class CborEnumItem extends CborStructItem {
    setValue(...args) {
       if (arguments.length > 0) {
         this.selector = arguments[0];
         this.record = arguments.slice(0,1);
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
       return [this.selector, ...arr_out];
    }

     fromData (json_data) {
       var arr_in = json_data.slice(0, 1); var arr_out = [];
       if (json_data[0] == 0) {
       }
       if (json_data[0] == 1) {
            arr_out.push((new FileCommand()).fromData(arr_in.shift());
       }
       if (json_data[0] == 2) {
            arr_out.push((new QueryCommand()).fromData(arr_in.shift());
       }
       this.selector = json_data[0];
       this.record = arr_out;
       }
}

Command.NoCommand = 0;   // no action requested
Command.CmdFc = 1;   // one of the file commands
Command.CmdQuery = 2;   // one of the query commands

