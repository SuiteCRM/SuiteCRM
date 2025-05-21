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

class Repository extends \Google\Model
{
  /**
   * @var string
   */
  public $createTime;
  protected $dataEncryptionStateType = DataEncryptionState::class;
  protected $dataEncryptionStateDataType = '';
  /**
   * @var string
   */
  public $displayName;
  protected $gitRemoteSettingsType = GitRemoteSettings::class;
  protected $gitRemoteSettingsDataType = '';
  /**
   * @var string
   */
  public $internalMetadata;
  /**
   * @var string
   */
  public $kmsKeyName;
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
  public $npmrcEnvironmentVariablesSecretVersion;
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var bool
   */
  public $setAuthenticatedUserAdmin;
  protected $workspaceCompilationOverridesType = WorkspaceCompilationOverrides::class;
  protected $workspaceCompilationOverridesDataType = '';

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
   * @param DataEncryptionState
   */
  public function setDataEncryptionState(DataEncryptionState $dataEncryptionState)
  {
    $this->dataEncryptionState = $dataEncryptionState;
  }
  /**
   * @return DataEncryptionState
   */
  public function getDataEncryptionState()
  {
    return $this->dataEncryptionState;
  }
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
   * @param GitRemoteSettings
   */
  public function setGitRemoteSettings(GitRemoteSettings $gitRemoteSettings)
  {
    $this->gitRemoteSettings = $gitRemoteSettings;
  }
  /**
   * @return GitRemoteSettings
   */
  public function getGitRemoteSettings()
  {
    return $this->gitRemoteSettings;
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
  public function setKmsKeyName($kmsKeyName)
  {
    $this->kmsKeyName = $kmsKeyName;
  }
  /**
   * @return string
   */
  public function getKmsKeyName()
  {
    return $this->kmsKeyName;
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
  public function setNpmrcEnvironmentVariablesSecretVersion($npmrcEnvironmentVariablesSecretVersion)
  {
    $this->npmrcEnvironmentVariablesSecretVersion = $npmrcEnvironmentVariablesSecretVersion;
  }
  /**
   * @return string
   */
  public function getNpmrcEnvironmentVariablesSecretVersion()
  {
    return $this->npmrcEnvironmentVariablesSecretVersion;
  }
  /**
   * @param string
   */
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
  /**
   * @param bool
   */
  public function setSetAuthenticatedUserAdmin($setAuthenticatedUserAdmin)
  {
    $this->setAuthenticatedUserAdmin = $setAuthenticatedUserAdmin;
  }
  /**
   * @return bool
   */
  public function getSetAuthenticatedUserAdmin()
  {
    return $this->setAuthenticatedUserAdmin;
  }
  /**
   * @param WorkspaceCompilationOverrides
   */
  public function setWorkspaceCompilationOverrides(WorkspaceCompilationOverrides $workspaceCompilationOverrides)
  {
    $this->workspaceCompilationOverrides = $workspaceCompilationOverrides;
  }
  /**
   * @return WorkspaceCompilationOverrides
   */
  public function getWorkspaceCompilationOverrides()
  {
    return $this->workspaceCompilationOverrides;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Repository::class, 'Google_Service_Dataform_Repository');
