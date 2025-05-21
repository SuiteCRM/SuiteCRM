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

namespace Google\Service\Bigquery;

class ScriptOptions extends \Google\Model
{
  /**
   * @var string
   */
  public $keyResultStatement;
  /**
   * @var string
   */
  public $statementByteBudget;
  /**
   * @var string
   */
  public $statementTimeoutMs;

  /**
   * @param string
   */
  public function setKeyResultStatement($keyResultStatement)
  {
    $this->keyResultStatement = $keyResultStatement;
  }
  /**
   * @return string
   */
  public function getKeyResultStatement()
  {
    return $this->keyResultStatement;
  }
  /**
   * @param string
   */
  public function setStatementByteBudget($statementByteBudget)
  {
    $this->statementByteBudget = $statementByteBudget;
  }
  /**
   * @return string
   */
  public function getStatementByteBudget()
  {
    return $this->statementByteBudget;
  }
  /**
   * @param string
   */
  public function setStatementTimeoutMs($statementTimeoutMs)
  {
    $this->statementTimeoutMs = $statementTimeoutMs;
  }
  /**
   * @return string
   */
  public function getStatementTimeoutMs()
  {
    return $this->statementTimeoutMs;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ScriptOptions::class, 'Google_Service_Bigquery_ScriptOptions');
