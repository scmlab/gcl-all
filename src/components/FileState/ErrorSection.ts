import { Position } from 'vscode-languageclient';
import { Error } from '../../data/Error'
import { ParseError } from '../../data/Error/ParseError';
import { StructError } from '../../data/Error/StructError';
import { TypeError } from '../../data/Error/TypeError';
import { renderRange } from '../Range';
import renderSection from '../Section'
import { renderPosition } from '../Position';

export default function renderError(error: Error): string {
  switch (error.tag) {
    case "ParseError":
      return renderParseError(error.message);
    case "TypeError":
      return renderTypeError(error.message);
    case "StructError":
      return renderStructError(error.message);
  }
}

function renderParseError(error: ParseError): string {
  switch (error.tag) {
    case "LexicalError":
      return renderSection("ParseError", `${error.tag}`, '', `at ${renderPosition(error.position)}`)
    case "SyntacticError":
      return renderSection("ParseError", `${error.tag}`, error.message, `at ${renderRange(error.location)}`)
  }
}

function renderTypeError(error: TypeError): string {
  let title: string = "TypeError";
  let subtitle: string = '';
  let code: string = `${error.tag}`;
  let sectionBody: string = ''
  switch (error.tag) {
    case "NotInScope":
      subtitle = `at ${renderRange(error.symbol.location.range)}`
      sectionBody = `Symbol \"${error.symbol.symbol}\" not in scope.`;
      break;
    case "UnifyFailed":
      subtitle = `at ${renderRange(error.location.range)}`
      sectionBody = `Failed when unifying type expressions \"${error.typeExpressions[0]}\" and \"${error.typeExpressions[1]}\"`;
      break;
    case "RecursiveType":
      subtitle = `at ${renderRange(error.typeVariable.location.range)}`;
      sectionBody = `Type variable \"${error.typeVariable.symbol}\" is recursive in ${error.typeExpression}`;
      break;
    case "AssignToConst":
      subtitle = `at ${renderRange(error.constSymbol.location.range)}`;
      sectionBody = `Assigning to const symbol \"${error.constSymbol}\"`;
      break;
    case "UndefinedType":
      subtitle = `at ${renderRange(error.typeVariable.location.range)}`;
      sectionBody = `Undefined type variable \"${error.typeVariable.symbol}\"`;
      break;
    case "DuplicatedIdentifiers":
      sectionBody = `Duplicated identifiers:${error.identifiers.map(identifier => `<br/>&nbsp;&nbsp;\"${identifier.symbol}\" at ${renderRange(identifier.location.range)}`)}`;
      break;
    case "RedundantNames":
      sectionBody = `Redundant names:${error.names.map(name => `<br/>&nbsp;&nbsp;\"${name.symbol}\" at ${renderRange(name.location.range)}`)}`;
      break;
    case "RedundantExprs":
      sectionBody = `Redundant expressions:${error.expressions.map(expression => `<br/>&nbsp;&nbsp;\"${expression}\"`)}`;
      break;
    case "MissingArguments":
      subtitle = `at ${renderRange(error.argumentNames[0].location.range)}`;
      sectionBody = `Missing arguments in a function call:${error.argumentNames.map(name => `<br/>&nbsp;&nbsp;\"${name}\"`)}`;
      break;
  }
  return renderSection(title, sectionBody, subtitle, code);
}

function renderStructError(error: StructError): string {
  return renderSection("StructError", "", `at ${renderRange(error.location.range)}`, error.tag);
}
