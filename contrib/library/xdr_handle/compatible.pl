
%% T-Area
compatible("string", "string").
compatible("id", "string").
compatible("idref", "string").
compatible("entity", "string").
compatible("nmtoken", "string").
compatible("notation", "string").
compatible("char", "string").
compatible("uri", "string").
compatible("bin.hex", "string").
compatible("bin.base64", "string").
compatible("dateTime", "string").
compatible("dateTime.tz", "string").
compatible("time.tz", "string").

%% Restrictions T-Area
compatible("textOnly", "string").

%% E-Area
compatible("enumeration", "enumeration").
compatible("entities", "enumeration").
compatible("nmtokens", "enumeration").
compatible("boolean", "enumeration").

%% N-Area
compatible("number", "number").
compatible("fixed.14.4", "number").
compatible("i1", "number").
compatible("byte", "number").
compatible("i2", "number").
compatible("i4", "number").
compatible("i8", "number").
compatible("ui1", "number").
compatible("ui2", "number").
compatible("ui4", "number").
compatible("ui8", "number").
compatible("r4", "number").
compatible("r8", "number").
compatible("float", "number").
compatible("uuid", "number").

%% YMD-Area
compatible("date", "date").

%% HMS-Area
compatible("time", "time").
