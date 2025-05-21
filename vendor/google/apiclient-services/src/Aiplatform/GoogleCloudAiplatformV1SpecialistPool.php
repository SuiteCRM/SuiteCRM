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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1SpecialistPool extends \Google\Collection
{
  protected $collection_key = 'specialistWorkerEmails';
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $pendingDataLabelingJobs;
  /**
   * @var string[]
   */
  public $specialistManagerEmails;
  /**
   * @var int
   */
  public $specialistManagersCount;
  /**
   * @var string[]
   */
  public $specialistWorkerEmails;

  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
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
   * @param string[]
   */
  public function setPendingDataLabelingJobs($pendingDataLabelingJobs)
  {
    $this->pendingDataLabelingJobs = $pendingDataLabelingJobs;
  }
  /**
   * @return string[]
   */
  public function getPendingDataLabelingJobs()
  {
    return $this->pendingDataLabelingJobs;
  }
  /**
   * @param string[]
   */
  public function setSpecialistManagerEmails($specialistManagerEmails)
  {
    $this->specialistManagerEmails = $specialistManagerEmails;
  }
  /**
   * @return string[]
   */
  public function getSpecialistManagerEmails()
  {
    return $this->specialistManagerEmails;
  }
  /**
   * @param int
   */
  public function setSpecialistManagersCount($specialistManagersCount)
  {
    $this->specialistManagersCount = $specialistManagersCount;
  }
  /**
   * @return int
   */
  public function getSpecialistManagersCount()
  {
    return $this->specialistManagersCount;
  }
  /**
   * @param string[]
   */
  public function setSpecialistWorkerEmails($specialistWorkerEmails)
  {
    $this->specialistWorkerEmails = $specialistWorkerEmails;
  }
  /**
   * @return string[]
   */
  public function getSpecialistWorkerEmails()
  {
    return $this->specialistWorkerEmails;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SpecialistPool::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SpecialistPool');
