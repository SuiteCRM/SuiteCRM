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

class WorkloadProfile extends \Google\Model
{
  protected $applicationType = Layer::class;
  protected $applicationDataType = '';
  protected $ascsType = Layer::class;
  protected $ascsDataType = '';
  protected $databaseType = Layer::class;
  protected $databaseDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $refreshedTime;
  protected $sapWorkloadType = SapWorkload::class;
  protected $sapWorkloadDataType = '';
  protected $sqlserverWorkloadType = SqlserverWorkload::class;
  protected $sqlserverWorkloadDataType = '';
  /**
   * @var string
   */
  public $state;
  protected $threeTierWorkloadType = ThreeTierWorkload::class;
  protected $threeTierWorkloadDataType = '';
  /**
   * @var string
   */
  public $workloadType;

  /**
   * @param Layer
   */
  public function setApplication(Layer $application)
  {
    $this->application = $application;
  }
  /**
   * @return Layer
   */
  public function getApplication()
  {
    return $this->application;
  }
  /**
   * @param Layer
   */
  public function setAscs(Layer $ascs)
  {
    $this->ascs = $ascs;
  }
  /**
   * @return Layer
   */
  public function getAscs()
  {
    return $this->ascs;
  }
  /**
   * @param Layer
   */
  public function setDatabase(Layer $database)
  {
    $this->database = $database;
  }
  /**
   * @return Layer
   */
  public function getDatabase()
  {
    return $this->database;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
  public function setRefreshedTime($refreshedTime)
  {
    $this->refreshedTime = $refreshedTime;
  }
  /**
   * @return string
   */
  public function getRefreshedTime()
  {
    return $this->refreshedTime;
  }
  /**
   * @param SapWorkload
   */
  public function setSapWorkload(SapWorkload $sapWorkload)
  {
    $this->sapWorkload = $sapWorkload;
  }
  /**
   * @return SapWorkload
   */
  public function getSapWorkload()
  {
    return $this->sapWorkload;
  }
  /**
   * @param SqlserverWorkload
   */
  public function setSqlserverWorkload(SqlserverWorkload $sqlserverWorkload)
  {
    $this->sqlserverWorkload = $sqlserverWorkload;
  }
  /**
   * @return SqlserverWorkload
   */
  public function getSqlserverWorkload()
  {
    return $this->sqlserverWorkload;
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
   * @param ThreeTierWorkload
   */
  public function setThreeTierWorkload(ThreeTierWorkload $threeTierWorkload)
  {
    $this->threeTierWorkload = $threeTierWorkload;
  }
  /**
   * @return ThreeTierWorkload
   */
  public function getThreeTierWorkload()
  {
    return $this->threeTierWorkload;
  }
  /**
   * @param string
   */
  public function setWorkloadType($workloadType)
  {
    $this->workloadType = $workloadType;
  }
  /**
   * @return string
   */
  public function getWorkloadType()
  {
    return $this->workloadType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(WorkloadProfile::class, 'Google_Service_WorkloadManager_WorkloadProfile');
