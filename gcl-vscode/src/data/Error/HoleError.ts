import { Range } from "vscode-languageclient";

export type HoleError = UnsatisfiedConstraint;

interface UnsatisfiedConstraint {
    tag: "UnsatisfiedConstraint";
    message: String;
    location?: Range;
}
