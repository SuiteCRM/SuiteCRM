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

namespace Google\Service\WorkloadManager;

class SapWorkload extends \Google\Model
{
  protected $applicationType = SapComponent::class;
  protected $applicationDataType = '';
  protected $databaseType = SapComponent::class;
  protected $databaseDataType = '';
  /**
   * @var string[]
   */
  public $metadata;

  /**
   * @param SapComponent
   */
  public function setApplication(SapComponent $application)
  {
    $this->application = $application;
  }
  /**
   * @return SapComponent
   */
  public function getApplication()
  {
    return $this->application;
  }
  /**
   * @param SapComponent
   */
  public function setDatabase(SapComponent $database)
  {
    $this->database = $database;
  }
  /**
   * @return SapComponent
   */
  public function getDatabase()
  {
    return $this->database;
  }
  /**
   * @param string[]
   */
  public function setMetadata($metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return string[]
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SapWorkload::class, 'Google_Service_WorkloadManager_SapWorkload');
