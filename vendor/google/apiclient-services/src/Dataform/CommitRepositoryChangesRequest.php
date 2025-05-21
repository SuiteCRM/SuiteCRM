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

class CommitRepositoryChangesRequest extends \Google\Model
{
  protected $commitMetadataType = CommitMetadata::class;
  protected $commitMetadataDataType = '';
  protected $fileOperationsType = FileOperation::class;
  protected $fileOperationsDataType = 'map';
  /**
   * @var string
   */
  public $requiredHeadCommitSha;

  /**
   * @param CommitMetadata
   */
  public function setCommitMetadata(CommitMetadata $commitMetadata)
  {
    $this->commitMetadata = $commitMetadata;
  }
  /**
   * @return CommitMetadata
   */
  public function getCommitMetadata()
  {
    return $this->commitMetadata;
  }
  /**
   * @param FileOperation[]
   */
  public function setFileOperations($fileOperations)
  {
    $this->fileOperations = $fileOperations;
  }
  /**
   * @return FileOperation[]
   */
  public function getFileOperations()
  {
    return $this->fileOperations;
  }
  /**
   * @param string
   */
  public function setRequiredHeadCommitSha($requiredHeadCommitSha)
  {
    $this->requiredHeadCommitSha = $requiredHeadCommitSha;
  }
  /**
   * @return string
   */
  public function getRequiredHeadCommitSha()
  {
    return $this->requiredHeadCommitSha;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CommitRepositoryChangesRequest::class, 'Google_Service_Dataform_CommitRepositoryChangesRequest');
