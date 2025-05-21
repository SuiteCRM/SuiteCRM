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

class Workload extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $folder;
  /**
   * @var string
   */
  public $folderId;
  /**
   * @var bool
   */
  public $isOnboarded;
  /**
   * @var string
   */
  public $keyManagementProjectId;
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $partner;
  protected $workloadOnboardingStateType = WorkloadOnboardingState::class;
  protected $workloadOnboardingStateDataType = '';

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setFolder($folder)
  {
    $this->folder = $folder;
  }
  /**
   * @return string
   */
  public function getFolder()
  {
    return $this->folder;
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
   * @param bool
   */
  public function setIsOnboarded($isOnboarded)
  {
    $this->isOnboarded = $isOnboarded;
  }
  /**
   * @return bool
   */
  public function getIsOnboarded()
  {
    return $this->isOnboarded;
  }
  /**
   * @param string
   */
  public function setKeyManagementProjectId($keyManagementProjectId)
  {
    $this->keyManagementProjectId = $keyManagementProjectId;
  }
  /**
   * @return string
   */
  public function getKeyManagementProjectId()
  {
    return $this->keyManagementProjectId;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
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
  public function setPartner($partner)
  {
    $this->partner = $partner;
  }
  /**
   * @return string
   */
  public function getPartner()
  {
    return $this->partner;
  }
  /**
   * @param WorkloadOnboardingState
   */
  public function setWorkloadOnboardingState(WorkloadOnboardingState $workloadOnboardingState)
  {
    $this->workloadOnboardingState = $workloadOnboardingState;
  }
  /**
   * @return WorkloadOnboardingState
   */
  public function getWorkloadOnboardingState()
  {
    return $this->workloadOnboardingState;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Workload::class, 'Google_Service_CloudControlsPartnerService_Workload');
