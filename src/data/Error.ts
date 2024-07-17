

import { ParseError } from "./Error/ParseError";
import { StructError } from "./Error/StructError";
import { TypeError } from "./Error/TypeError";

export type Error = {
    tag: "ParseError";
    message: ParseError
} | {
    tag: "TypeError";
    message: TypeError;
} | {
    tag: "StructError";
    message: StructError;
}
