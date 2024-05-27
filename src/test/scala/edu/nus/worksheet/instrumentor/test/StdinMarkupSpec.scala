package edu.nus.worksheet.instrumentor.test;

import org.scalatest.*;
import flatspec.*;

import edu.nus.worksheet.instrumentor.*;
import edu.nus.worksheet.*;

class StdinMarkupSpec extends AnyFlatSpec {
  "Stdin Markup" should "read basic //IN: comments" in {
    val src = """//IN:
// one
// two
"""
    val expected = Seq("one", "two");
    val actual = StdinMarkup.extractFromSource(src);

    assertResult(expected)(actual);
  }

  it should "read basic /*IN: .. */ comments" in {
    val src = """/*IN:
one
two
*/"""
    val expected = Seq("one", "two");
    val actual = StdinMarkup.extractFromSource(src);

    assertResult(expected)(actual);
  }

  it should "read fancy-style /*IN: .. */ comments" in {
    val src = """
/*IN:
 * one
 * two
 */"""
    val expected = Seq("one", "two");
    val actual = StdinMarkup.extractFromSource(src);

    assertResult(expected)(actual);
  }

  it should "not be strict about */ for /*IN: .. */ comments" in {
    val src = """
/*IN:
 * one
 * two
 **/"""
    val expected = Seq("one", "two");
    val actual = StdinMarkup.extractFromSource(src);

    assertResult(expected)(actual);
  }

  it should "ignore non-comments" in {
    val src = """//IN:
// one
// two
int x;
/*IN:
three
four
*/
int main(int argc, char** argv) {
  return 0;
}
"""
    val expected = Seq("one", "two", "three", "four");
    val actual = StdinMarkup.extractFromSource(src);

    assertResult(expected)(actual);
  }
}
