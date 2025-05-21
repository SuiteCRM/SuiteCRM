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

namespace Google\Service\Storage;

class BulkRestoreObjectsRequest extends \Google\Collection
{
  protected $collection_key = 'matchGlobs';
  /**
   * @var bool
   */
  public $allowOverwrite;
  /**
   * @var bool
   */
  public $copySourceAcl;
  /**
   * @var string[]
   */
  public $matchGlobs;
  /**
   * @var string
   */
  public $softDeletedAfterTime;
  /**
   * @var string
   */
  public $softDeletedBeforeTime;

  /**
   * @param bool
   */
  public function setAllowOverwrite($allowOverwrite)
  {
    $this->allowOverwrite = $allowOverwrite;
  }
  /**
   * @return bool
   */
  public function getAllowOverwrite()
  {
    return $this->allowOverwrite;
  }
  /**
   * @param bool
   */
  public function setCopySourceAcl($copySourceAcl)
  {
    $this->copySourceAcl = $copySourceAcl;
  }
  /**
   * @return bool
   */
  public function getCopySourceAcl()
  {
    return $this->copySourceAcl;
  }
  /**
   * @param string[]
   */
  public function setMatchGlobs($matchGlobs)
  {
    $this->matchGlobs = $matchGlobs;
  }
  /**
   * @return string[]
   */
  public function getMatchGlobs()
  {
    return $this->matchGlobs;
  }
  /**
   * @param string
   */
  public function setSoftDeletedAfterTime($softDeletedAfterTime)
  {
    $this->softDeletedAfterTime = $softDeletedAfterTime;
  }
  /**
   * @return string
   */
  public function getSoftDeletedAfterTime()
  {
    return $this->softDeletedAfterTime;
  }
  /**
   * @param string
   */
  public function setSoftDeletedBeforeTime($softDeletedBeforeTime)
  {
    $this->softDeletedBeforeTime = $softDeletedBeforeTime;
  }
  /**
   * @return string
   */
  public function getSoftDeletedBeforeTime()
  {
    return $this->softDeletedBeforeTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BulkRestoreObjectsRequest::class, 'Google_Service_Storage_BulkRestoreObjectsRequest');
