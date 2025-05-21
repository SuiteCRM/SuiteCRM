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

namespace Google\Service\CloudControlsPartnerService;

class Violation extends \Google\Model
{
  /**
   * @var string
   */
  public $beginTime;
  /**
   * @var string
   */
  public $category;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $folderId;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $nonCompliantOrgPolicy;
  protected $remediationType = Remediation::class;
  protected $remediationDataType = '';
  /**
   * @var string
   */
  public $resolveTime;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setBeginTime($beginTime)
  {
    $this->beginTime = $beginTime;
  }
  /**
   * @return string
   */
  public function getBeginTime()
  {
    return $this->beginTime;
  }
  /**
   * @param string
   */
  public function setCategory($category)
  {
    $this->category = $category;
  }
  /**
   * @return string
   */
  public function getCategory()
  {
    return $this->category;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setFolderId($folderId)
  {
    $this->folderId = $folderId;
  }
  /**
   * @return string
   */
  public function getFolderId()
  {
    return $this->folderId;
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
  public function setNonCompliantOrgPolicy($nonCompliantOrgPolicy)
  {
    $this->nonCompliantOrgPolicy = $nonCompliantOrgPolicy;
  }
  /**
   * @return string
   */
  public function getNonCompliantOrgPolicy()
  {
    return $this->nonCompliantOrgPolicy;
  }
  /**
   * @param Remediation
   */
  public function setRemediation(Remediation $remediation)
  {
    $this->remediation = $remediation;
  }
  /**
   * @return Remediation
   */
  public function getRemediation()
  {
    return $this->remediation;
  }
  /**
   * @param string
   */
  public function setResolveTime($resolveTime)
  {
    $this->resolveTime = $resolveTime;
  }
  /**
   * @return string
   */
  public function getResolveTime()
  {
    return $this->resolveTime;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Violation::class, 'Google_Service_CloudControlsPartnerService_Violation');
