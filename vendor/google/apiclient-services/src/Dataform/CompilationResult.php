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

class CompilationResult extends \Google\Collection
{
  protected $collection_key = 'compilationErrors';
  protected $codeCompilationConfigType = CodeCompilationConfig::class;
  protected $codeCompilationConfigDataType = '';
  protected $compilationErrorsType = CompilationError::class;
  protected $compilationErrorsDataType = 'array';
  /**
   * @var string
   */
  public $createTime;
  protected $dataEncryptionStateType = DataEncryptionState::class;
  protected $dataEncryptionStateDataType = '';
  /**
   * @var string
   */
  public $dataformCoreVersion;
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
  /**
   * @var string
   */
  public $releaseConfig;
  /**
   * @var string
   */
  public $resolvedGitCommitSha;
  /**
   * @var string
   */
  public $workspace;

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
   * @param CompilationError[]
   */
  public function setCompilationErrors($compilationErrors)
  {
    $this->compilationErrors = $compilationErrors;
  }
  /**
   * @return CompilationError[]
   */
  public function getCompilationErrors()
  {
    return $this->compilationErrors;
  }
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
  public function setDataformCoreVersion($dataformCoreVersion)
  {
    $this->dataformCoreVersion = $dataformCoreVersion;
  }
  /**
   * @return string
   */
  public function getDataformCoreVersion()
  {
    return $this->dataformCoreVersion;
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
   * @param string
   */
  public function setReleaseConfig($releaseConfig)
  {
    $this->releaseConfig = $releaseConfig;
  }
  /**
   * @return string
   */
  public function getReleaseConfig()
  {
    return $this->releaseConfig;
  }
  /**
   * @param string
   */
  public function setResolvedGitCommitSha($resolvedGitCommitSha)
  {
    $this->resolvedGitCommitSha = $resolvedGitCommitSha;
  }
  /**
   * @return string
   */
  public function getResolvedGitCommitSha()
  {
    return $this->resolvedGitCommitSha;
  }
  /**
   * @param string
   */
  public function setWorkspace($workspace)
  {
    $this->workspace = $workspace;
  }
  /**
   * @return string
   */
  public function getWorkspace()
  {
    return $this->workspace;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CompilationResult::class, 'Google_Service_Dataform_CompilationResult');
