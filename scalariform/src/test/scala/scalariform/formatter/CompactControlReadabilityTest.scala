package scalariform.formatter

import preferences.FormattingPreferences._
import scalariform.formatter.preferences._
import scalariform.parser._
import scalariform.formatter._


class CompactControlReadabilityTest extends AbstractExpressionFormatterTest{

  override val debug = false

  string2FormatTest(
    """if(a){
      |foo
      |} else {
      |bar }""")(FormattingPreferences().setPreference(CompactControlReadability, true)) ==>
  """if (a) {
    |  foo
    |}
    |else {
    |  bar
    |}"""

  string2FormatTest(
    """if(a){
      |foo
      |}
      |else {
      |bar }""")(FormattingPreferences().setPreference(CompactControlReadability, true)) ==>
  """if (a) {
    |  foo
    |}
    |else {
    |  bar
    |}"""

  string2FormatTest(
    """if(a){
      |foo
      |} else {
      |
      |bar }""")(FormattingPreferences().setPreference(CompactControlReadability, true)) ==>
  """if (a) {
    |  foo
    |}
    |else {
    |
    |  bar
    |}"""

  string2FormatTest(
    """try{
      |  foo
      |} catch {
      |  bar
      |}""")(FormattingPreferences().setPreference(CompactControlReadability, true)) ==>
  """try {
    |  foo
    |}
    |catch {
    |  bar
    |}"""

  string2FormatTest(
    """try{
      |  foo
      |} finally {
      |  bar
      |}""")(FormattingPreferences().setPreference(CompactControlReadability, true)) ==>
  """try {
    |  foo
    |}
    |finally {
    |  bar
    |}"""

  string2FormatTest(
    """try{
      |  foo
      |}
      |finally {
      |  bar
      |}""")(FormattingPreferences().setPreference(CompactControlReadability, true)) ==>
  """try {
    |  foo
    |}
    |finally {
    |  bar
    |}"""

  string2FormatTest(
    """try{
      |  foo
      |} finally {
      |
      |  bar
      |}""")(FormattingPreferences().setPreference(CompactControlReadability, true)) ==>
  """try {
    |  foo
    |}
    |finally {
    |
    |  bar
    |}"""
}