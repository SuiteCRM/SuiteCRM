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

namespace Google\Service\CloudComposer;

class LoadSnapshotRequest extends \Google\Model
{
  /**
   * @var bool
   */
  public $skipAirflowOverridesSetting;
  /**
   * @var bool
   */
  public $skipEnvironmentVariablesSetting;
  /**
   * @var bool
   */
  public $skipGcsDataCopying;
  /**
   * @var bool
   */
  public $skipPypiPackagesInstallation;
  /**
   * @var string
   */
  public $snapshotPath;

  /**
   * @param bool
   */
  public function setSkipAirflowOverridesSetting($skipAirflowOverridesSetting)
  {
    $this->skipAirflowOverridesSetting = $skipAirflowOverridesSetting;
  }
  /**
   * @return bool
   */
  public function getSkipAirflowOverridesSetting()
  {
    return $this->skipAirflowOverridesSetting;
  }
  /**
   * @param bool
   */
  public function setSkipEnvironmentVariablesSetting($skipEnvironmentVariablesSetting)
  {
    $this->skipEnvironmentVariablesSetting = $skipEnvironmentVariablesSetting;
  }
  /**
   * @return bool
   */
  public function getSkipEnvironmentVariablesSetting()
  {
    return $this->skipEnvironmentVariablesSetting;
  }
  /**
   * @param bool
   */
  public function setSkipGcsDataCopying($skipGcsDataCopying)
  {
    $this->skipGcsDataCopying = $skipGcsDataCopying;
  }
  /**
   * @return bool
   */
  public function getSkipGcsDataCopying()
  {
    return $this->skipGcsDataCopying;
  }
  /**
   * @param bool
   */
  public function setSkipPypiPackagesInstallation($skipPypiPackagesInstallation)
  {
    $this->skipPypiPackagesInstallation = $skipPypiPackagesInstallation;
  }
  /**
   * @return bool
   */
  public function getSkipPypiPackagesInstallation()
  {
    return $this->skipPypiPackagesInstallation;
  }
  /**
   * @param string
   */
  public function setSnapshotPath($snapshotPath)
  {
    $this->snapshotPath = $snapshotPath;
  }
  /**
   * @return string
   */
  public function getSnapshotPath()
  {
    return $this->snapshotPath;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(LoadSnapshotRequest::class, 'Google_Service_CloudComposer_LoadSnapshotRequest');
