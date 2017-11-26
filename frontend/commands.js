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

    static fromCBOR (ser_data) {
       var json_data = CBOR.decode(ser_data);
       return fromData(json_data);
    }

    toCBOR () {
       var data = this.toData();
       return CBOR.encode(data);
    }

    toData () {
       return this.record;
    }
}

class CborEnumItem {

    constructor(s, ...args) {
       this.setValue(s, ...args);
    }

    setValue(s, ...args) {
       this.selector = s;
       this.record = args;
    }

    static fromCBOR (ser_data) {
       var json_data = CBOR.decode(ser_data);
       return fromData(json_data);
    }

    toCBOR () {
       var data = this.toData();
       return CBOR.encode(data);
    }

    toData () {
       return [this.selector, ...this.record];
    }
}

class FileCommand extends CborEnumItem {

    constructor(s, ...args) {
       super(s, ...args);
    }

    static fromData (json_data) {
       var arr = json_data;
       return new FileCommand(...arr);
    }
}

FileCommand.GetCurrentDB = 0;
FileCommand.GetAvailableDBs = 1;
FileCommand.CreateDB = 2;
FileCommand.DeleteDB = 3;
FileCommand.OpenDB = 4;
FileCommand.SaveDB = 5;
FileCommand.SaveDBAs = 6;

class QueryCommand extends CborEnumItem {

    constructor(s, ...args) {
       super(s, ...args);
    }

    static fromData (json_data) {
       var arr = json_data;
       return new QueryCommand(...arr);
    }
}

QueryCommand.DbInfo = 0;
QueryCommand.DbQuery = 1;

class Command extends CborEnumItem {

    constructor(s, ...args) {
       super(s, ...args);
    }

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

    static fromData (json_data) {
       var arr_in = json_data.slice(0, 1); var arr_out = [];
       if (json_data[0] == 0) {
       }
       if (json_data[0] == 1) {
            arr_out.push(arr_in.shift().fromData());
       }
       if (json_data[0] == 2) {
            arr_out.push(arr_in.shift().fromData());
       }
       var arr = [json_data[0], ...arr_out];
       return new Command(...arr);
    }
}

Command.NoCommand = 0;
Command.CmdFc = 1;
Command.CmdQuery = 2;


module.exports = { QueryCommand: QueryCommand, FileCommand: FileCommand, Command: Command };

}) (this);

