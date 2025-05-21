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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV2ExternalSystem extends \Google\Collection
{
  protected $collection_key = 'assignees';
  /**
   * @var string[]
   */
  public $assignees;
  /**
   * @var string
   */
  public $caseCloseTime;
  /**
   * @var string
   */
  public $caseCreateTime;
  /**
   * @var string
   */
  public $casePriority;
  /**
   * @var string
   */
  public $caseSla;
  /**
   * @var string
   */
  public $caseUri;
  /**
   * @var string
   */
  public $externalSystemUpdateTime;
  /**
   * @var string
   */
  public $externalUid;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $status;
  protected $ticketInfoType = GoogleCloudSecuritycenterV2TicketInfo::class;
  protected $ticketInfoDataType = '';

  /**
   * @param string[]
   */
  public function setAssignees($assignees)
  {
    $this->assignees = $assignees;
  }
  /**
   * @return string[]
   */
  public function getAssignees()
  {
    return $this->assignees;
  }
  /**
   * @param string
   */
  public function setCaseCloseTime($caseCloseTime)
  {
    $this->caseCloseTime = $caseCloseTime;
  }
  /**
   * @return string
   */
  public function getCaseCloseTime()
  {
    return $this->caseCloseTime;
  }
  /**
   * @param string
   */
  public function setCaseCreateTime($caseCreateTime)
  {
    $this->caseCreateTime = $caseCreateTime;
  }
  /**
   * @return string
   */
  public function getCaseCreateTime()
  {
    return $this->caseCreateTime;
  }
  /**
   * @param string
   */
  public function setCasePriority($casePriority)
  {
    $this->casePriority = $casePriority;
  }
  /**
   * @return string
   */
  public function getCasePriority()
  {
    return $this->casePriority;
  }
  /**
   * @param string
   */
  public function setCaseSla($caseSla)
  {
    $this->caseSla = $caseSla;
  }
  /**
   * @return string
   */
  public function getCaseSla()
  {
    return $this->caseSla;
  }
  /**
   * @param string
   */
  public function setCaseUri($caseUri)
  {
    $this->caseUri = $caseUri;
  }
  /**
   * @return string
   */
  public function getCaseUri()
  {
    return $this->caseUri;
  }
  /**
   * @param string
   */
  public function setExternalSystemUpdateTime($externalSystemUpdateTime)
  {
    $this->externalSystemUpdateTime = $externalSystemUpdateTime;
  }
  /**
   * @return string
   */
  public function getExternalSystemUpdateTime()
  {
    return $this->externalSystemUpdateTime;
  }
  /**
   * @param string
   */
  public function setExternalUid($externalUid)
  {
    $this->externalUid = $externalUid;
  }
  /**
   * @return string
   */
  public function getExternalUid()
  {
    return $this->externalUid;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param GoogleCloudSecuritycenterV2TicketInfo
   */
  public function setTicketInfo(GoogleCloudSecuritycenterV2TicketInfo $ticketInfo)
  {
    $this->ticketInfo = $ticketInfo;
  }
  /**
   * @return GoogleCloudSecuritycenterV2TicketInfo
   */
  public function getTicketInfo()
  {
    return $this->ticketInfo;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2ExternalSystem::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2ExternalSystem');
