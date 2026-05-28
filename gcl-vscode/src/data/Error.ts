import { Range } from "vscode-languageclient";
import { ParseError } from "./Error/ParseError";
import { StructError } from "./Error/StructError";
import { TypeError } from "./Error/TypeError";
import { HoleError } from "./Error/HoleError";

export type Error =
  | {
      tag: "CannotReadFile";
      filePath: string;
    }
  | {
      tag: "ParseError";
      message: ParseError;
    }
  | {
      tag: "TypeError";
      message: TypeError;
    }
  | {
      tag: "StructError";
      message: StructError;
    }
  | {
      tag: "HoleError";
      message: HoleError;
    }
  | {
      tag: "Others";
      title: string;
      message: string;
      location?: Range;
    };
