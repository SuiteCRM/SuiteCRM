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

class EnterpriseTopazSidekickPeopleAnswerRelatedPeopleAnswerCard extends \Google\Collection
{
  protected $collection_key = 'relatedPeople';
  protected $disambiguationInfoType = EnterpriseTopazSidekickPeopleAnswerDisambiguationInfo::class;
  protected $disambiguationInfoDataType = '';
  protected $headerType = EnterpriseTopazSidekickPeopleAnswerPeopleAnswerCardHeader::class;
  protected $headerDataType = '';
  protected $relatedPeopleType = EnterpriseTopazSidekickCommonPerson::class;
  protected $relatedPeopleDataType = 'array';
  /**
   * @var string
   */
  public $relationType;
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
   * @param EnterpriseTopazSidekickCommonPerson[]
   */
  public function setRelatedPeople($relatedPeople)
  {
    $this->relatedPeople = $relatedPeople;
  }
  /**
   * @return EnterpriseTopazSidekickCommonPerson[]
   */
  public function getRelatedPeople()
  {
    return $this->relatedPeople;
  }
  /**
   * @param string
   */
  public function setRelationType($relationType)
  {
    $this->relationType = $relationType;
  }
  /**
   * @return string
   */
  public function getRelationType()
  {
    return $this->relationType;
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
class_alias(EnterpriseTopazSidekickPeopleAnswerRelatedPeopleAnswerCard::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickPeopleAnswerRelatedPeopleAnswerCard');
