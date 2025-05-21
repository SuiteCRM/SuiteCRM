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

namespace Google\Service\SQLAdmin;

class InstancesRestoreBackupRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $backup;
  protected $restoreBackupContextType = RestoreBackupContext::class;
  protected $restoreBackupContextDataType = '';
  protected $restoreInstanceSettingsType = DatabaseInstance::class;
  protected $restoreInstanceSettingsDataType = '';

  /**
   * @param string
   */
  public function setBackup($backup)
  {
    $this->backup = $backup;
  }
  /**
   * @return string
   */
  public function getBackup()
  {
    return $this->backup;
  }
  /**
   * @param RestoreBackupContext
   */
  public function setRestoreBackupContext(RestoreBackupContext $restoreBackupContext)
  {
    $this->restoreBackupContext = $restoreBackupContext;
  }
  /**
   * @return RestoreBackupContext
   */
  public function getRestoreBackupContext()
  {
    return $this->restoreBackupContext;
  }
  /**
   * @param DatabaseInstance
   */
  public function setRestoreInstanceSettings(DatabaseInstance $restoreInstanceSettings)
  {
    $this->restoreInstanceSettings = $restoreInstanceSettings;
  }
  /**
   * @return DatabaseInstance
   */
  public function getRestoreInstanceSettings()
  {
    return $this->restoreInstanceSettings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InstancesRestoreBackupRequest::class, 'Google_Service_SQLAdmin_InstancesRestoreBackupRequest');
