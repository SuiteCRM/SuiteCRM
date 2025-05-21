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

namespace Google\Service\CloudRun;

class GoogleDevtoolsCloudbuildV1SourceProvenance extends \Google\Model
{
  protected $fileHashesType = GoogleDevtoolsCloudbuildV1FileHashes::class;
  protected $fileHashesDataType = 'map';
  protected $resolvedConnectedRepositoryType = GoogleDevtoolsCloudbuildV1ConnectedRepository::class;
  protected $resolvedConnectedRepositoryDataType = '';
  protected $resolvedGitSourceType = GoogleDevtoolsCloudbuildV1GitSource::class;
  protected $resolvedGitSourceDataType = '';
  protected $resolvedRepoSourceType = GoogleDevtoolsCloudbuildV1RepoSource::class;
  protected $resolvedRepoSourceDataType = '';
  protected $resolvedStorageSourceType = GoogleDevtoolsCloudbuildV1StorageSource::class;
  protected $resolvedStorageSourceDataType = '';
  protected $resolvedStorageSourceManifestType = GoogleDevtoolsCloudbuildV1StorageSourceManifest::class;
  protected $resolvedStorageSourceManifestDataType = '';

  /**
   * @param GoogleDevtoolsCloudbuildV1FileHashes[]
   */
  public function setFileHashes($fileHashes)
  {
    $this->fileHashes = $fileHashes;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1FileHashes[]
   */
  public function getFileHashes()
  {
    return $this->fileHashes;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1ConnectedRepository
   */
  public function setResolvedConnectedRepository(GoogleDevtoolsCloudbuildV1ConnectedRepository $resolvedConnectedRepository)
  {
    $this->resolvedConnectedRepository = $resolvedConnectedRepository;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1ConnectedRepository
   */
  public function getResolvedConnectedRepository()
  {
    return $this->resolvedConnectedRepository;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1GitSource
   */
  public function setResolvedGitSource(GoogleDevtoolsCloudbuildV1GitSource $resolvedGitSource)
  {
    $this->resolvedGitSource = $resolvedGitSource;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1GitSource
   */
  public function getResolvedGitSource()
  {
    return $this->resolvedGitSource;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1RepoSource
   */
  public function setResolvedRepoSource(GoogleDevtoolsCloudbuildV1RepoSource $resolvedRepoSource)
  {
    $this->resolvedRepoSource = $resolvedRepoSource;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1RepoSource
   */
  public function getResolvedRepoSource()
  {
    return $this->resolvedRepoSource;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1StorageSource
   */
  public function setResolvedStorageSource(GoogleDevtoolsCloudbuildV1StorageSource $resolvedStorageSource)
  {
    $this->resolvedStorageSource = $resolvedStorageSource;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1StorageSource
   */
  public function getResolvedStorageSource()
  {
    return $this->resolvedStorageSource;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1StorageSourceManifest
   */
  public function setResolvedStorageSourceManifest(GoogleDevtoolsCloudbuildV1StorageSourceManifest $resolvedStorageSourceManifest)
  {
    $this->resolvedStorageSourceManifest = $resolvedStorageSourceManifest;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1StorageSourceManifest
   */
  public function getResolvedStorageSourceManifest()
  {
    return $this->resolvedStorageSourceManifest;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleDevtoolsCloudbuildV1SourceProvenance::class, 'Google_Service_CloudRun_GoogleDevtoolsCloudbuildV1SourceProvenance');
