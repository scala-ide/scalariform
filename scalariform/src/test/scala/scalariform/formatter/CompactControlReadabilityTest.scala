package scalariform.formatter

import scalariform.formatter.preferences._

// format: OFF
class CompactControlReadabilityTest extends AbstractExpressionFormatterTest {

  implicit val formattingPreferences = FormattingPreferences.setPreference(CompactControlReadability, true)

  """if(a){
    |foo
    |} else {
    |bar }""" ==>
  """if (a) {
    |  foo
    |}
    |else {
    |  bar
    |}"""

  """if(a){
    |foo
    |}
    |else {
    |bar }""" ==>
  """if (a) {
    |  foo
    |}
    |else {
    |  bar
    |}"""

  """if(a){
    |foo
    |} else {
    |
    |bar }""" ==>
  """if (a) {
    |  foo
    |}
    |else {
    |
    |  bar
    |}"""

  """try{
    |  foo
    |} catch {
    |  bar
    |}""" ==>
  """try {
    |  foo
    |}
    |catch {
    |  bar
    |}"""

  """try{
    |  foo
    |} finally {
    |  bar
    |}""" ==>
  """try {
    |  foo
    |}
    |finally {
    |  bar
    |}"""

  """try{
    |  foo
    |}
    |finally {
    |  bar
    |}""" ==>
  """try {
    |  foo
    |}
    |finally {
    |  bar
    |}"""

  """try{
    |  foo
    |} finally {
    |
    |  bar
    |}""" ==>
  """try {
    |  foo
    |}
    |finally {
    |
    |  bar
    |}"""

  "if (y > 0) positive else if (y < 0) negative else zero" ==> "if (y > 0) positive else if (y < 0) negative else zero"

  "try x catch y finally z" ==> "try x catch y finally z"

}
