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

namespace Google\Service\Dataform;

class ReleaseConfig extends \Google\Collection
{
  protected $collection_key = 'recentScheduledReleaseRecords';
  protected $codeCompilationConfigType = CodeCompilationConfig::class;
  protected $codeCompilationConfigDataType = '';
  /**
   * @var string
   */
  public $cronSchedule;
  /**
   * @var bool
   */
  public $disabled;
  /**
   * @var string
   */
  public $gitCommitish;
  /**
   * @var string
   */
  public $internalMetadata;
  /**
   * @var string
   */
  public $name;
  protected $recentScheduledReleaseRecordsType = ScheduledReleaseRecord::class;
  protected $recentScheduledReleaseRecordsDataType = 'array';
  /**
   * @var string
   */
  public $releaseCompilationResult;
  /**
   * @var string
   */
  public $timeZone;

  /**
   * @param CodeCompilationConfig
   */
  public function setCodeCompilationConfig(CodeCompilationConfig $codeCompilationConfig)
  {
    $this->codeCompilationConfig = $codeCompilationConfig;
  }
  /**
   * @return CodeCompilationConfig
   */
  public function getCodeCompilationConfig()
  {
    return $this->codeCompilationConfig;
  }
  /**
   * @param string
   */
  public function setCronSchedule($cronSchedule)
  {
    $this->cronSchedule = $cronSchedule;
  }
  /**
   * @return string
   */
  public function getCronSchedule()
  {
    return $this->cronSchedule;
  }
  /**
   * @param bool
   */
  public function setDisabled($disabled)
  {
    $this->disabled = $disabled;
  }
  /**
   * @return bool
   */
  public function getDisabled()
  {
    return $this->disabled;
  }
  /**
   * @param string
   */
  public function setGitCommitish($gitCommitish)
  {
    $this->gitCommitish = $gitCommitish;
  }
  /**
   * @return string
   */
  public function getGitCommitish()
  {
    return $this->gitCommitish;
  }
  /**
   * @param string
   */
  public function setInternalMetadata($internalMetadata)
  {
    $this->internalMetadata = $internalMetadata;
  }
  /**
   * @return string
   */
  public function getInternalMetadata()
  {
    return $this->internalMetadata;
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
   * @param ScheduledReleaseRecord[]
   */
  public function setRecentScheduledReleaseRecords($recentScheduledReleaseRecords)
  {
    $this->recentScheduledReleaseRecords = $recentScheduledReleaseRecords;
  }
  /**
   * @return ScheduledReleaseRecord[]
   */
  public function getRecentScheduledReleaseRecords()
  {
    return $this->recentScheduledReleaseRecords;
  }
  /**
   * @param string
   */
  public function setReleaseCompilationResult($releaseCompilationResult)
  {
    $this->releaseCompilationResult = $releaseCompilationResult;
  }
  /**
   * @return string
   */
  public function getReleaseCompilationResult()
  {
    return $this->releaseCompilationResult;
  }
  /**
   * @param string
   */
  public function setTimeZone($timeZone)
  {
    $this->timeZone = $timeZone;
  }
  /**
   * @return string
   */
  public function getTimeZone()
  {
    return $this->timeZone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ReleaseConfig::class, 'Google_Service_Dataform_ReleaseConfig');
