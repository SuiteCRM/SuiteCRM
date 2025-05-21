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

namespace Google\Service\Spanner;

class IndexAdvice extends \Google\Collection
{
  protected $collection_key = 'ddl';
  /**
   * @var string[]
   */
  public $ddl;
  public $improvementFactor;

  /**
   * @param string[]
   */
  public function setDdl($ddl)
  {
    $this->ddl = $ddl;
  }
  /**
   * @return string[]
   */
  public function getDdl()
  {
    return $this->ddl;
  }
  public function setImprovementFactor($improvementFactor)
  {
    $this->improvementFactor = $improvementFactor;
  }
  public function getImprovementFactor()
  {
    return $this->improvementFactor;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IndexAdvice::class, 'Google_Service_Spanner_IndexAdvice');
