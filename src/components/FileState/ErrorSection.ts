import { Position } from 'vscode-languageclient';
import { Error } from '../../data/Error'
import { ParseError } from '../../data/Error/ParseError';
import { StructError } from '../../data/Error/StructError';
import { TypeError } from '../../data/Error/TypeError';
import { renderRange } from '../Range';
import renderSection from '../Section'
import { renderPosition } from '../Position';

function renderErrorSection(title: string, sectionBody: string, subtitle?: string, code?: string): string {
  return renderSection(/*html*/`<span style="color: var(--vscode-editorError-foreground)">${title}</span>`, sectionBody, subtitle, code)
}

export default function renderError(error: Error): string {
  switch (error.tag) {
    case "CannotReadFile":
      return renderErrorSection("CannotReadFile", error.filePath);
    case "ParseError":
      return renderParseError(error.message);
    case "TypeError":
      return renderTypeError(error.message);
    case "StructError":
      return renderStructError(error.message);
    case "Others":
      return renderErrorSection("Error", error.message)
  }
}

function renderParseError(error: ParseError): string {
  switch (error.tag) {
    case "LexicalError":
      return renderErrorSection("ParseError", `${error.tag}`, '', `at ${renderPosition(error.position)}`)
    case "SyntacticError":
      return renderErrorSection("ParseError", `
        ${error.tag}:<br>
        ${error.locatedSymbols.map(({location, symbol}) => `${location? `at ${renderRange(location)}: ` : ''}${symbol}`).join('<br>')}`, '', '')
  }
}

function renderTypeError(error: TypeError): string {
  let title: string = "TypeError";
  let subtitle: string = '';
  let code: string = `${error.tag}`;
  let sectionBody: string = ''
  switch (error.tag) {
    case "NotInScope":
      if (error.symbol.location) subtitle = `${(renderRange(error.symbol.location))}`;
      sectionBody = `Symbol \"${error.symbol.symbol}\" not in scope.`;
      break;
    case "UnifyFailed":
      if (error.location) subtitle = `at ${renderRange(error.location)}`;
      sectionBody = `Failed when unifying type expressions \"${error.typeExpressions[0]}\" and \"${error.typeExpressions[1]}\"`;
      break;
    case "RecursiveType":
      if (error.typeVariable.location) subtitle = `at ${renderRange(error.typeVariable.location)}`;
      sectionBody = `Type variable \"${error.typeVariable.symbol}\" is recursive in ${error.typeExpression}`;
      break;
    case "AssignToConst":
      if (error.constSymbol.location) subtitle = `at ${renderRange(error.constSymbol.location)}`;
      sectionBody = `Assigning to const symbol \"${error.constSymbol}\"`;
      break;
    case "UndefinedType":
      if (error.typeVariable.location) subtitle = `at ${renderRange(error.typeVariable.location)}`;
      sectionBody = `Undefined type variable \"${error.typeVariable.symbol}\"`;
      break;
    case "DuplicatedIdentifiers":
      sectionBody = `Duplicated identifiers:${error.identifiers.map(identifier => `<br/>&nbsp;&nbsp;\"${identifier.symbol}\"${identifier.location? ` at ${renderRange(identifier.location)}`: ''}`)}`;
      break;
    case "RedundantNames":
      sectionBody = `Redundant names:${error.names.map(name => `<br/>&nbsp;&nbsp;\"${name.symbol}\"${name.location? ` at ${renderRange(name.location)}`: ''}`)}`;
      break;
    case "RedundantExprs":
      sectionBody = `Redundant expressions:${error.expressions.map(expression => `<br/>&nbsp;&nbsp;\"${expression}\"`)}`;
      break;
    case "MissingArguments":
      if (error.argumentNames[0].location) subtitle = `at ${renderRange(error.argumentNames[0].location)}`;
      sectionBody = `Missing arguments in a function call:${error.argumentNames.map(name => `<br/>&nbsp;&nbsp;\"${name}\"`)}`;
      break;
    case "KindUnifyFailed":
      if (error.location) subtitle = `at ${renderRange(error.location)}`;
      sectionBody = `Failed when unifying kind expressions \"${error.kindExpressions[0]}\" and \"${error.kindExpressions[1]}\"`;
      break;
    case "PatternArityMismatch":
      if (error.location) subtitle = `at ${renderRange(error.location)}`;
      sectionBody = `Expecting ${error.expected} arguments but received ${error.received}`;
      break;

  }
  return renderErrorSection(title, sectionBody, subtitle, code);
}

function renderStructError(error: StructError): string {
  return renderErrorSection("StructError", "", error.location? `at ${renderRange(error.location.range)}`: "", error.tag);
}
