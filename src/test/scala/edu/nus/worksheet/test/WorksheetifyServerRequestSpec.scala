package edu.nus.worksheet.test;

import argonaut.Parse;

import org.scalatest.*;
import flatspec.*;

import edu.nus.worksheet.WorksheetifyServer;
import WorksheetifyServer.{Request, requestCodecJson};

class WorksheetifyServerRequestSpec extends AnyFlatSpec {

  "WorksheetifyServer.Request" should "decode JSON" in {
    val expectedReq =
      Request("file", "./hello.c", "json-outputlist", None, None);

    val requestJson =
      """{"inputtype":"file", "input":"./hello.c", "outputtype":"json-outputlist"}""";
    val actualReq = Parse.decodeOption[Request](requestJson.toString).get;

    assertResult(expectedReq)(actualReq);
  }

}
