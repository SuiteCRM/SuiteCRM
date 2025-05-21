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

namespace Google\Service\ApigeeRegistry;

class Build extends \Google\Model
{
  /**
   * @var string
   */
  public $commitId;
  /**
   * @var string
   */
  public $commitTime;
  /**
   * @var string
   */
  public $repo;

  /**
   * @param string
   */
  public function setCommitId($commitId)
  {
    $this->commitId = $commitId;
  }
  /**
   * @return string
   */
  public function getCommitId()
  {
    return $this->commitId;
  }
  /**
   * @param string
   */
  public function setCommitTime($commitTime)
  {
    $this->commitTime = $commitTime;
  }
  /**
   * @return string
   */
  public function getCommitTime()
  {
    return $this->commitTime;
  }
  /**
   * @param string
   */
  public function setRepo($repo)
  {
    $this->repo = $repo;
  }
  /**
   * @return string
   */
  public function getRepo()
  {
    return $this->repo;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Build::class, 'Google_Service_ApigeeRegistry_Build');
