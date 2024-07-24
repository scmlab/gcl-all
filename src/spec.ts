// This module handles specs for holes.

import * as vscode from 'vscode';

// The class for specs.
class Spec {
	constructor(public pre: string, public post: string, public range: vscode.Range) { }
}
