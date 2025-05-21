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

namespace Google\Service\Contactcenterinsights;

class GoogleCloudContactcenterinsightsV1RuntimeAnnotationUserInput extends \Google\Model
{
  /**
   * @var string
   */
  public $generatorName;
  /**
   * @var string
   */
  public $query;
  /**
   * @var string
   */
  public $querySource;

  /**
   * @param string
   */
  public function setGeneratorName($generatorName)
  {
    $this->generatorName = $generatorName;
  }
  /**
   * @return string
   */
  public function getGeneratorName()
  {
    return $this->generatorName;
  }
  /**
   * @param string
   */
  public function setQuery($query)
  {
    $this->query = $query;
  }
  /**
   * @return string
   */
  public function getQuery()
  {
    return $this->query;
  }
  /**
   * @param string
   */
  public function setQuerySource($querySource)
  {
    $this->querySource = $querySource;
  }
  /**
   * @return string
   */
  public function getQuerySource()
  {
    return $this->querySource;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1RuntimeAnnotationUserInput::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1RuntimeAnnotationUserInput');
