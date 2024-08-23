

import { ParseError } from "./Error/ParseError";
import { StructError } from "./Error/StructError";
import { TypeError } from "./Error/TypeError";
import { Loc } from "./Loc";

export type Error = {
    tag: "CannotReadFile";
    filePath: string;
} | {
    tag: "ParseError";
    message: ParseError
} | {
    tag: "TypeError";
    message: TypeError;
} | {
    tag: "StructError";
    message: StructError;
} | {
    tag: "Others";
    title: string;
    message: string;
    location?: Loc;
} 
