

import { ParseError } from "./Error/ParseError";
import { StructError } from "./Error/StructError";
import { TypeError } from "./Error/TypeError";

export type Error = ParseError | TypeError | StructError;