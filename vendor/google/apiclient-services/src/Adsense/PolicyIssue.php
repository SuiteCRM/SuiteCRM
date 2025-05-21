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

namespace Google\Service\Adsense;

class PolicyIssue extends \Google\Collection
{
  protected $collection_key = 'policyTopics';
  /**
   * @var string
   */
  public $action;
  /**
   * @var string[]
   */
  public $adClients;
  /**
   * @var string
   */
  public $adRequestCount;
  /**
   * @var string
   */
  public $entityType;
  protected $firstDetectedDateType = Date::class;
  protected $firstDetectedDateDataType = '';
  protected $lastDetectedDateType = Date::class;
  protected $lastDetectedDateDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $policyTopicsType = PolicyTopic::class;
  protected $policyTopicsDataType = 'array';
  /**
   * @var string
   */
  public $site;
  /**
   * @var string
   */
  public $siteSection;
  /**
   * @var string
   */
  public $uri;
  protected $warningEscalationDateType = Date::class;
  protected $warningEscalationDateDataType = '';

  /**
   * @param string
   */
  public function setAction($action)
  {
    $this->action = $action;
  }
  /**
   * @return string
   */
  public function getAction()
  {
    return $this->action;
  }
  /**
   * @param string[]
   */
  public function setAdClients($adClients)
  {
    $this->adClients = $adClients;
  }
  /**
   * @return string[]
   */
  public function getAdClients()
  {
    return $this->adClients;
  }
  /**
   * @param string
   */
  public function setAdRequestCount($adRequestCount)
  {
    $this->adRequestCount = $adRequestCount;
  }
  /**
   * @return string
   */
  public function getAdRequestCount()
  {
    return $this->adRequestCount;
  }
  /**
   * @param string
   */
  public function setEntityType($entityType)
  {
    $this->entityType = $entityType;
  }
  /**
   * @return string
   */
  public function getEntityType()
  {
    return $this->entityType;
  }
  /**
   * @param Date
   */
  public function setFirstDetectedDate(Date $firstDetectedDate)
  {
    $this->firstDetectedDate = $firstDetectedDate;
  }
  /**
   * @return Date
   */
  public function getFirstDetectedDate()
  {
    return $this->firstDetectedDate;
  }
  /**
   * @param Date
   */
  public function setLastDetectedDate(Date $lastDetectedDate)
  {
    $this->lastDetectedDate = $lastDetectedDate;
  }
  /**
   * @return Date
   */
  public function getLastDetectedDate()
  {
    return $this->lastDetectedDate;
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
   * @param PolicyTopic[]
   */
  public function setPolicyTopics($policyTopics)
  {
    $this->policyTopics = $policyTopics;
  }
  /**
   * @return PolicyTopic[]
   */
  public function getPolicyTopics()
  {
    return $this->policyTopics;
  }
  /**
   * @param string
   */
  public function setSite($site)
  {
    $this->site = $site;
  }
  /**
   * @return string
   */
  public function getSite()
  {
    return $this->site;
  }
  /**
   * @param string
   */
  public function setSiteSection($siteSection)
  {
    $this->siteSection = $siteSection;
  }
  /**
   * @return string
   */
  public function getSiteSection()
  {
    return $this->siteSection;
  }
  /**
   * @param string
   */
  public function setUri($uri)
  {
    $this->uri = $uri;
  }
  /**
   * @return string
   */
  public function getUri()
  {
    return $this->uri;
  }
  /**
   * @param Date
   */
  public function setWarningEscalationDate(Date $warningEscalationDate)
  {
    $this->warningEscalationDate = $warningEscalationDate;
  }
  /**
   * @return Date
   */
  public function getWarningEscalationDate()
  {
    return $this->warningEscalationDate;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PolicyIssue::class, 'Google_Service_Adsense_PolicyIssue');
