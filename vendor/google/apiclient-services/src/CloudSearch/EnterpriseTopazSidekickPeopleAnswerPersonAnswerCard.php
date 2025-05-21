<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\CloudSearch;

class EnterpriseTopazSidekickPeopleAnswerPersonAnswerCard extends \Google\Collection
{
  protected $collection_key = 'answer';
  protected $answerType = SafeHtmlProto::class;
  protected $answerDataType = 'array';
  protected $answerTextType = EnterpriseTopazSidekickAnswerAnswerList::class;
  protected $answerTextDataType = '';
  protected $disambiguationInfoType = EnterpriseTopazSidekickPeopleAnswerDisambiguationInfo::class;
  protected $disambiguationInfoDataType = '';
  protected $headerType = EnterpriseTopazSidekickPeopleAnswerPeopleAnswerCardHeader::class;
  protected $headerDataType = '';
  /**
   * @var string
   */
  public $responseStatus;
  /**
   * @var string
   */
  public $statusMessage;
  protected $subjectType = EnterpriseTopazSidekickCommonPerson::class;
  protected $subjectDataType = '';

  /**
   * @param SafeHtmlProto[]
   */
  public function setAnswer($answer)
  {
    $this->answer = $answer;
  }
  /**
   * @return SafeHtmlProto[]
   */
  public function getAnswer()
  {
    return $this->answer;
  }
  /**
   * @param EnterpriseTopazSidekickAnswerAnswerList
   */
  public function setAnswerText(EnterpriseTopazSidekickAnswerAnswerList $answerText)
  {
    $this->answerText = $answerText;
  }
  /**
   * @return EnterpriseTopazSidekickAnswerAnswerList
   */
  public function getAnswerText()
  {
    return $this->answerText;
  }
  /**
   * @param EnterpriseTopazSidekickPeopleAnswerDisambiguationInfo
   */
  public function setDisambiguationInfo(EnterpriseTopazSidekickPeopleAnswerDisambiguationInfo $disambiguationInfo)
  {
    $this->disambiguationInfo = $disambiguationInfo;
  }
  /**
   * @return EnterpriseTopazSidekickPeopleAnswerDisambiguationInfo
   */
  public function getDisambiguationInfo()
  {
    return $this->disambiguationInfo;
  }
  /**
   * @param EnterpriseTopazSidekickPeopleAnswerPeopleAnswerCardHeader
   */
  public function setHeader(EnterpriseTopazSidekickPeopleAnswerPeopleAnswerCardHeader $header)
  {
    $this->header = $header;
  }
  /**
   * @return EnterpriseTopazSidekickPeopleAnswerPeopleAnswerCardHeader
   */
  public function getHeader()
  {
    return $this->header;
  }
  /**
   * @param string
   */
  public function setResponseStatus($responseStatus)
  {
    $this->responseStatus = $responseStatus;
  }
  /**
   * @return string
   */
  public function getResponseStatus()
  {
    return $this->responseStatus;
  }
  /**
   * @param string
   */
  public function setStatusMessage($statusMessage)
  {
    $this->statusMessage = $statusMessage;
  }
  /**
   * @return string
   */
  public function getStatusMessage()
  {
    return $this->statusMessage;
  }
  /**
   * @param EnterpriseTopazSidekickCommonPerson
   */
  public function setSubject(EnterpriseTopazSidekickCommonPerson $subject)
  {
    $this->subject = $subject;
  }
  /**
   * @return EnterpriseTopazSidekickCommonPerson
   */
  public function getSubject()
  {
    return $this->subject;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnterpriseTopazSidekickPeopleAnswerPersonAnswerCard::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickPeopleAnswerPersonAnswerCard');
