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

namespace Google\Service\ArtifactRegistry;

class GoogleDevtoolsArtifactregistryV1RemoteRepositoryConfigYumRepositoryPublicRepository extends \Google\Model
{
  /**
   * @var string
   */
  public $repositoryBase;
  /**
   * @var string
   */
  public $repositoryPath;

  /**
   * @param string
   */
  public function setRepositoryBase($repositoryBase)
  {
    $this->repositoryBase = $repositoryBase;
  }
  /**
   * @return string
   */
  public function getRepositoryBase()
  {
    return $this->repositoryBase;
  }
  /**
   * @param string
   */
  public function setRepositoryPath($repositoryPath)
  {
    $this->repositoryPath = $repositoryPath;
  }
  /**
   * @return string
   */
  public function getRepositoryPath()
  {
    return $this->repositoryPath;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleDevtoolsArtifactregistryV1RemoteRepositoryConfigYumRepositoryPublicRepository::class, 'Google_Service_ArtifactRegistry_GoogleDevtoolsArtifactregistryV1RemoteRepositoryConfigYumRepositoryPublicRepository');
