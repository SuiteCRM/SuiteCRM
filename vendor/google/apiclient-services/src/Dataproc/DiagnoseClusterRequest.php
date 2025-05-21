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

namespace Google\Service\Dataproc;

class DiagnoseClusterRequest extends \Google\Collection
{
  protected $collection_key = 'yarnApplicationIds';
  protected $diagnosisIntervalType = Interval::class;
  protected $diagnosisIntervalDataType = '';
  /**
   * @var string
   */
  public $job;
  /**
   * @var string[]
   */
  public $jobs;
  /**
   * @var string
   */
  public $tarballAccess;
  /**
   * @var string
   */
  public $tarballGcsDir;
  /**
   * @var string
   */
  public $yarnApplicationId;
  /**
   * @var string[]
   */
  public $yarnApplicationIds;

  /**
   * @param Interval
   */
  public function setDiagnosisInterval(Interval $diagnosisInterval)
  {
    $this->diagnosisInterval = $diagnosisInterval;
  }
  /**
   * @return Interval
   */
  public function getDiagnosisInterval()
  {
    return $this->diagnosisInterval;
  }
  /**
   * @param string
   */
  public function setJob($job)
  {
    $this->job = $job;
  }
  /**
   * @return string
   */
  public function getJob()
  {
    return $this->job;
  }
  /**
   * @param string[]
   */
  public function setJobs($jobs)
  {
    $this->jobs = $jobs;
  }
  /**
   * @return string[]
   */
  public function getJobs()
  {
    return $this->jobs;
  }
  /**
   * @param string
   */
  public function setTarballAccess($tarballAccess)
  {
    $this->tarballAccess = $tarballAccess;
  }
  /**
   * @return string
   */
  public function getTarballAccess()
  {
    return $this->tarballAccess;
  }
  /**
   * @param string
   */
  public function setTarballGcsDir($tarballGcsDir)
  {
    $this->tarballGcsDir = $tarballGcsDir;
  }
  /**
   * @return string
   */
  public function getTarballGcsDir()
  {
    return $this->tarballGcsDir;
  }
  /**
   * @param string
   */
  public function setYarnApplicationId($yarnApplicationId)
  {
    $this->yarnApplicationId = $yarnApplicationId;
  }
  /**
   * @return string
   */
  public function getYarnApplicationId()
  {
    return $this->yarnApplicationId;
  }
  /**
   * @param string[]
   */
  public function setYarnApplicationIds($yarnApplicationIds)
  {
    $this->yarnApplicationIds = $yarnApplicationIds;
  }
  /**
   * @return string[]
   */
  public function getYarnApplicationIds()
  {
    return $this->yarnApplicationIds;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DiagnoseClusterRequest::class, 'Google_Service_Dataproc_DiagnoseClusterRequest');
