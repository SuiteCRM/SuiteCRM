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

namespace Google\Service\CloudDeploy;

class DeployArtifact extends \Google\Collection
{
  protected $collection_key = 'manifestPaths';
  /**
   * @var string
   */
  public $artifactUri;
  /**
   * @var string[]
   */
  public $manifestPaths;

  /**
   * @param string
   */
  public function setArtifactUri($artifactUri)
  {
    $this->artifactUri = $artifactUri;
  }
  /**
   * @return string
   */
  public function getArtifactUri()
  {
    return $this->artifactUri;
  }
  /**
   * @param string[]
   */
  public function setManifestPaths($manifestPaths)
  {
    $this->manifestPaths = $manifestPaths;
  }
  /**
   * @return string[]
   */
  public function getManifestPaths()
  {
    return $this->manifestPaths;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DeployArtifact::class, 'Google_Service_CloudDeploy_DeployArtifact');
